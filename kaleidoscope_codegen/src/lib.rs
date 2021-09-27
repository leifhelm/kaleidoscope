use kaleidoscope_ast::*;

use std::path::Path;
use std::sync::Arc;
use std::{borrow::Borrow, collections::HashMap};

use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    passes::PassManager,
    support::LLVMString,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple},
    types::BasicTypeEnum,
    values::{BasicValue, BasicValueEnum, FloatValue, FunctionValue},
    AddressSpace, OptimizationLevel,
};
use thread_local::ThreadLocal;

struct CodeGen<'ctx, X> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    function_pass_manager: PassManager<FunctionValue<'ctx>>,
    variables: HashMap<String, FloatValue<'ctx>>,
    errors: Vec<CodeGenError<'ctx, X>>,
    ast_list: &'ctx Vec<AST<X>>,
}

impl<'ctx, X> CodeGen<'ctx, X> {
    fn compile(mut self) -> Result<Module<'ctx>, Vec<CodeGenError<'ctx, X>>> {
        for ast_value in self.ast_list {
            match ast_value {
                AST::Expression(expr) => {
                    self.add_result(self.toplevel_expression(&expr));
                }
                AST::ExternFunction(prototype) => {
                    self.function_prototype(prototype);
                }
                AST::Function(function) => {
                    let function = self.function(&function);
                    self.add_result(function);
                }
            }
        }
        // Add return statement
        if let Some(main) = self.module.get_function("main") {
            let entry = main.get_first_basic_block().unwrap();
            self.builder.position_at_end(entry);
            self.builder
                .build_return(Some(&self.context.i32_type().const_int(0, true)));
            if !main.verify(true) {
                self.errors.push(CodeGenError::InvalidMainFunction);
            }
        }
        self.add_result(
            self.module
                .verify()
                .map_err(CodeGenError::InvalidGeneratedModule),
        );
        self.module.print_to_stderr();
        if self.errors.is_empty() {
            Ok(self.module)
        } else {
            Err(self.errors)
        }
    }
    fn add_result<T>(&mut self, result: Result<T, CodeGenError<'ctx, X>>) {
        match result {
            Ok(_) => (),
            Err(err) => self.errors.push(err),
        }
    }
    fn toplevel_expression(
        &self,
        expression: &'ctx Expression<X>,
    ) -> Result<(), CodeGenError<'ctx, X>> {
        let printf = match self.module.get_function("printf") {
            Some(function) => function,
            None => self.module.add_function(
                "printf",
                self.context.i32_type().fn_type(
                    &vec![self
                        .context
                        .i8_type()
                        .ptr_type(AddressSpace::Generic)
                        .into()],
                    true,
                ),
                Some(Linkage::External),
            ),
        };
        let main = match self.module.get_function("main") {
            Some(function) => function,
            None => self.module.add_function(
                "main",
                self.context.i32_type().fn_type(&vec![], false),
                None,
            ),
        };
        let entry = match main.get_first_basic_block() {
            Some(block) => block,
            None => self.context.append_basic_block(main, "entry"),
        };
        self.builder.position_at_end(entry);
        let printf_str = self
            .builder
            .build_global_string_ptr("%f\n", "printf_str")
            .as_basic_value_enum();
        let expr = self.expression(expression)?;
        self.builder
            .build_call(printf, &vec![printf_str, expr.into()], "printtmp");
        Ok(())
    }
    fn expression(
        &self,
        expression: &'ctx Expression<X>,
    ) -> Result<FloatValue<'ctx>, CodeGenError<'ctx, X>> {
        match expression {
            Expression::LiteralNumber(val) => Ok(self.context.f64_type().const_float(val.wrapped)),
            Expression::Variable(name) => match self.variables.get(&name.wrapped) {
                Some(variable) => Ok(*variable),
                None => Err(CodeGenError::VariableNotFound(name.clone())),
            },
            Expression::BinaryOperation(bin_op) => {
                let lhs = self.expression(&bin_op.left_hand_side)?;
                let rhs = self.expression(&bin_op.right_hand_side)?;
                match bin_op.operation.wrapped {
                    Op::Add => Ok(self.builder.build_float_add(lhs, rhs, "addtmp")),
                    Op::Subtract => Ok(self.builder.build_float_sub(lhs, rhs, "subtmp")),
                    Op::Multiply => Ok(self.builder.build_float_mul(lhs, rhs, "multmp")),
                    Op::Divide => Ok(self.builder.build_float_div(lhs, rhs, "divtmp")),
                }
            }
            Expression::Call(Call { callee, args }) => {
                if let Some(callee_function) = self.module.get_function(&callee.wrapped) {
                    let cf_arg_len = callee_function.count_params();
                    if cf_arg_len != args.len() as u32 {
                        return Err(CodeGenError::ArgumentLengthMismatch {
                            function_name: callee.clone(),
                            expected: cf_arg_len,
                            actual: args.len(),
                        });
                    }
                    let mut args_v: Vec<BasicValueEnum> = Vec::with_capacity(args.len());
                    for arg in args {
                        args_v.push(self.expression(&arg)?.into());
                    }
                    match self
                        .builder
                        .build_call(callee_function, args_v.as_slice(), "calltmp")
                        .try_as_basic_value()
                        .left()
                    {
                        Some(value) => Ok(value.into_float_value()),
                        None => Err(CodeGenError::InvalidFunctionCall(callee.clone())),
                    }
                } else {
                    Err(CodeGenError::FunctionNotFound(callee.clone()))
                }
            }
        }
    }
    fn function_prototype(&self, prototype: &'ctx FunctionPrototype<X>) -> FunctionValue<'ctx> {
        let doubles: Vec<BasicTypeEnum> =
            vec![self.context.f64_type().into(); prototype.args.len()];
        let fn_type = self.context.f64_type().fn_type(&doubles, false);
        let function =
            self.module
                .add_function(&prototype.name.wrapped, fn_type, Some(Linkage::External));
        for (i, arg_name) in prototype.args.iter().enumerate() {
            function.get_params()[i].set_name(&arg_name.wrapped);
        }
        function
    }
    fn function(
        &mut self,
        function_dec: &'ctx Function<X>,
    ) -> Result<FunctionValue<'ctx>, CodeGenError<'ctx, X>> {
        let proto = &function_dec.prototype;
        let function = self
            .module
            .get_function(&proto.name.wrapped)
            .unwrap_or_else(|| self.function_prototype(proto));
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);
        self.variables.clear();
        self.variables.reserve(proto.args.len());
        for arg in function.get_param_iter() {
            self.variables
                .insert(name_of_basic_value(&arg), arg.into_float_value());
        }
        let body = self.expression(&function_dec.body)?;
        self.builder.build_return(Some(&body));
        if function.verify(true) {
            self.function_pass_manager.run_on(&function);
            Ok(function)
        } else {
            unsafe {
                function.delete();
            }
            Err(CodeGenError::InvalidGeneratedFunction(&proto.name))
        }
    }
}

fn name_of_basic_value(value: &BasicValueEnum) -> String {
    match value {
        BasicValueEnum::ArrayValue(val) => val.get_name(),
        BasicValueEnum::IntValue(val) => val.get_name(),
        BasicValueEnum::FloatValue(val) => val.get_name(),
        BasicValueEnum::PointerValue(val) => val.get_name(),
        BasicValueEnum::StructValue(val) => val.get_name(),
        BasicValueEnum::VectorValue(val) => val.get_name(),
    }
    .to_string_lossy()
    .into_owned()
}
pub fn codegen<'ctx, X>(
    global_context: &'ctx GlobalContext,
    ast_list: &'ctx Vec<AST<X>>,
) -> Result<CodeGenUnit<'ctx>, Vec<CodeGenError<'ctx, X>>> {
    let context = global_context.get_llvm_context();
    let module = context.create_module("kaleidoscope");
    let function_pass_manager = PassManager::create(module.borrow());
    function_pass_manager.add_instruction_combining_pass();
    function_pass_manager.add_reassociate_pass();
    function_pass_manager.add_gvn_pass();
    function_pass_manager.add_cfg_simplification_pass();
    function_pass_manager.initialize();
    let builder = context.create_builder();
    let codegen = CodeGen {
        context: &context,
        module,
        builder,
        function_pass_manager,
        variables: HashMap::new(),
        errors: Vec::new(),
        ast_list,
    };
    codegen.compile().map(|module| CodeGenUnit { module })
}

pub fn write_executable<'ctx>(cu: CodeGenUnit<'ctx>) -> Result<(), String> {
    Target::initialize_x86(&InitializationConfig::default());

    let target = Target::from_name("x86-64").unwrap();
    let target_machine = target
        .create_target_machine(
            &TargetTriple::create("x86_64-pc-linux-gnu"),
            "x86-64",
            "+avx2",
            OptimizationLevel::Default,
            RelocMode::Default,
            CodeModel::Default,
        )
        .unwrap();
    target_machine
        .write_to_file(&cu.module, FileType::Object, Path::new("a.o"))
        .map_err(|err| err.to_string())
}

pub struct CodeGenUnit<'ctx> {
    module: Module<'ctx>,
}

#[derive(Debug, PartialEq)]
pub enum CodeGenError<'ctx, X> {
    VariableNotFound(&'ctx Identifier<X>),
    FunctionNotFound(&'ctx Identifier<X>),
    ArgumentLengthMismatch {
        function_name: &'ctx Identifier<X>,
        expected: u32,
        actual: usize,
    },
    InvalidFunctionCall(&'ctx Identifier<X>),
    InvalidGeneratedFunction(&'ctx Identifier<X>),
    InvalidMainFunction,
    InvalidGeneratedModule(LLVMString),
}

pub struct GlobalContext {
    llvm_context: Arc<ThreadLocal<Context>>,
}

impl GlobalContext {
    pub fn new() -> GlobalContext {
        GlobalContext {
            llvm_context: Arc::new(ThreadLocal::new()),
        }
    }
    fn get_llvm_context(&self) -> &Context {
        self.llvm_context.get_or(|| Context::create())
    }
}

impl Clone for GlobalContext {
    fn clone(&self) -> Self {
        Self {
            llvm_context: self.llvm_context.clone(),
        }
    }
}

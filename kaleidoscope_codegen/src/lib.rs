use inkwell::basic_block::BasicBlock;
use kaleidoscope_ast::*;

use std::ops::Deref;
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
    types::BasicMetadataTypeEnum,
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FloatValue, FunctionValue},
    AddressSpace, FloatPredicate, OptimizationLevel,
};
use thread_local::ThreadLocal;

struct CodeGen<'ctx, X> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    function_pass_manager: PassManager<FunctionValue<'ctx>>,
    errors: Vec<CodeGenError<'ctx, X>>,
    ast_list: &'ctx Vec<AST<X>>,
}

impl<'ctx, X> CodeGen<'ctx, X> {
    fn compile(mut self) -> Result<Module<'ctx>, Vec<CodeGenError<'ctx, X>>> {
        let mut s = MutableState {
            variables: HashMap::new(),
            main_bb: None,
            main_function: None,
        };
        for ast_value in self.ast_list {
            s = match ast_value {
                AST::Expression(expr) => {
                    let expression = self.toplevel_expression(s, &expr);
                    self.add_result(expression)
                }
                AST::ExternFunction(prototype) => {
                    self.function_prototype(prototype);
                    s
                }
                AST::Function(function) => {
                    let function = self.function(s, &function);
                    self.add_result(function)
                }
            }
        }
        if !self.errors.is_empty() {
            return Err(self.errors);
        }
        // Add return statement
        if let (Some(main), Some(main_bb)) = (s.main_function, s.main_bb) {
            self.builder.position_at_end(main_bb);
            self.builder
                .build_return(Some(&self.context.i32_type().const_int(0, true)));
            if !main.verify(true) {
                self.errors.push(CodeGenError::InvalidMainFunction);
            }
        }
        if let Err(err) = self.module.verify() {
            self.errors.push(CodeGenError::InvalidGeneratedModule(err));
        }
        self.module.print_to_stderr();
        if self.errors.is_empty() {
            Ok(self.module)
        } else {
            Err(self.errors)
        }
    }
    fn add_result<T>(
        &mut self,
        result: SResult<'ctx, T, CodeGenError<'ctx, X>>,
    ) -> MutableState<'ctx> {
        match result {
            Ok((s, _)) => s,
            Err((s, err)) => {
                self.errors.push(err);
                s
            }
        }
    }
    fn toplevel_expression(
        &self,
        mut s: MutableState<'ctx>,
        expression: &'ctx Expression<X>,
    ) -> SResult<'ctx, (), CodeGenError<'ctx, X>> {
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
        let main = match s.main_function {
            Some(function) => function,
            None => {
                let main_function = self.module.add_function(
                    "main",
                    self.context.i32_type().fn_type(&vec![], false),
                    None,
                );
                s.main_function = Some(main_function);
                main_function
            }
        };
        let block = match s.main_bb {
            Some(block) => block,
            None => {
                let main_bb = self.context.append_basic_block(main, "entry");
                s.main_bb = Some(main_bb);
                main_bb
            }
        };
        self.builder.position_at_end(block);
        let printf_str = self
            .builder
            .build_global_string_ptr("%f\n", "printf_str")
            .as_basic_value_enum()
            .into();
        let (s, expr) = self.expression(s, expression)?;
        let (mut s, bb) = map(s, Some, self.get_insert_block(expression))?;
        s.main_bb = bb;
        let args = vec![printf_str, expr.into()];
        self.builder.build_call(printf, &args[..], "printtmp");
        Ok((s, ()))
    }
    fn expression(
        &self,
        mut s: MutableState<'ctx>,
        expression: &'ctx Expression<X>,
    ) -> SResult<'ctx, FloatValue<'ctx>, CodeGenError<'ctx, X>> {
        match expression.deref() {
            Expr::LiteralNumber(val) => Ok((s, self.context.f64_type().const_float(val.wrapped))),
            Expr::Variable(name) => match s.get_variable(&name.wrapped) {
                (s, Some(variable)) => Ok((s, variable)),
                (s, None) => Err((s, CodeGenError::VariableNotFound(name.clone()))),
            },
            Expr::BinaryOperation(bin_op) => {
                let (s, lhs) = self.expression(s, &bin_op.left_hand_side)?;
                let (s, rhs) = self.expression(s, &bin_op.right_hand_side)?;
                match bin_op.operation.wrapped {
                    Op::Add => Ok((s, self.builder.build_float_add(lhs, rhs, "addtmp"))),
                    Op::Subtract => Ok((s, self.builder.build_float_sub(lhs, rhs, "subtmp"))),
                    Op::Multiply => Ok((s, self.builder.build_float_mul(lhs, rhs, "multmp"))),
                    Op::Divide => Ok((s, self.builder.build_float_div(lhs, rhs, "divtmp"))),
                    Op::Greater => Ok((s, self.build_compare(FloatPredicate::OGT, lhs, rhs))),
                    Op::GreaterOrEqual => {
                        Ok((s, self.build_compare(FloatPredicate::OGE, lhs, rhs)))
                    }
                    Op::Less => Ok((s, self.build_compare(FloatPredicate::OLT, lhs, rhs))),
                    Op::LessOrEqual => Ok((s, self.build_compare(FloatPredicate::OLE, lhs, rhs))),
                    Op::Equal => Ok((s, self.build_compare(FloatPredicate::OEQ, lhs, rhs))),
                    Op::NotEqual => Ok((s, self.build_compare(FloatPredicate::ONE, lhs, rhs))),
                }
            }
            Expr::Call(Call { callee, args }) => {
                if let Some(callee_function) = self.module.get_function(&callee.wrapped) {
                    let cf_arg_len = callee_function.count_params();
                    if cf_arg_len != args.len() as u32 {
                        return Err((
                            s,
                            CodeGenError::ArgumentLengthMismatch {
                                function_name: callee.clone(),
                                expected: cf_arg_len,
                                actual: args.len(),
                            },
                        ));
                    }
                    let mut args_v: Vec<BasicMetadataValueEnum> = Vec::with_capacity(args.len());
                    for arg in args {
                        let (st, expr) = self.expression(s, &arg)?;
                        args_v.push(expr.into());
                        s = st;
                    }
                    match self
                        .builder
                        .build_call(callee_function, args_v.as_slice(), "calltmp")
                        .try_as_basic_value()
                        .left()
                    {
                        Some(value) => Ok((s, value.into_float_value())),
                        None => Err((s, CodeGenError::InvalidFunctionCall(callee.clone()))),
                    }
                } else {
                    Err((s, CodeGenError::FunctionNotFound(callee.clone())))
                }
            }
            Expr::If(if_expression) => {
                let IfExpression {
                    condition,
                    then,
                    r#else,
                } = if_expression.as_ref();
                let (s, condition_val) = self.expression(s, condition)?;
                let condition_val = self.builder.build_float_compare(
                    FloatPredicate::ONE,
                    condition_val,
                    self.context.f64_type().const_float(0.0),
                    "ifcond",
                );
                let (s, function) = wrap(s, self.get_function(expression))?;
                let then_bb = self.context.append_basic_block(function, "then");
                let else_bb = self.context.append_basic_block(function, "else");
                let merge_bb = self.context.append_basic_block(function, "merge");
                self.builder
                    .build_conditional_branch(condition_val, then_bb, else_bb);
                self.builder.position_at_end(then_bb);
                let (s, then_val) = self.expression(s, then)?;
                self.builder.build_unconditional_branch(merge_bb);
                let (s, then_bb) = wrap(s, self.get_insert_block(expression))?;
                self.builder.position_at_end(else_bb);
                let (s, else_val) = self.expression(s, r#else)?;
                self.builder.build_unconditional_branch(merge_bb);
                let (s, else_bb) = wrap(s, self.get_insert_block(expression))?;
                self.builder.position_at_end(merge_bb);
                let phi = self.builder.build_phi(self.context.f64_type(), "iftmp");
                let phi_incomming = vec![
                    (&then_val as &dyn BasicValue, then_bb),
                    (&else_val, else_bb),
                ];
                phi.add_incoming(&phi_incomming);
                Ok((s, phi.as_basic_value().into_float_value()))
            }
            Expr::For(for_expression) => {
                let ForExpression {
                    variable,
                    start,
                    end,
                    step,
                    body,
                } = for_expression.as_ref();
                let (s, start_val) = self.expression(s, start)?;
                let (s, function) = wrap(s, self.get_function(expression))?;
                let loop_bb = self.context.append_basic_block(function, "loop");
                self.builder.build_unconditional_branch(loop_bb);
                let (mut s, preheader_bb) = wrap(s, self.get_insert_block(expression))?;
                self.builder.position_at_end(loop_bb);
                let variable_val = self
                    .builder
                    .build_phi(self.context.f64_type(), &variable.wrapped);
                let variable_old_val = s.variables.insert(
                    variable.wrapped.clone(),
                    variable_val.as_basic_value().into_float_value(),
                );
                let (s, body_val) = self.expression(s, body)?;
                let (s, step_val) = match step {
                    Some(step) => self.expression(s, step)?,
                    None => (s, self.context.f64_type().const_float(1.0)),
                };
                let next_var = self.builder.build_float_add(
                    variable_val.as_basic_value().into_float_value(),
                    step_val.as_basic_value_enum().into_float_value(),
                    "nextvar",
                );
                let (s, end_cond) = self.expression(s, end)?;
                let end_cond = self.builder.build_float_compare(
                    FloatPredicate::ONE,
                    end_cond,
                    self.context.f64_type().const_float(0.0),
                    "loopcond",
                );
                let (mut s, loop_end_bb) = wrap(s, self.get_insert_block(expression))?;
                let phi_incomming = vec![
                    (&start_val as &dyn BasicValue, preheader_bb),
                    (&next_var as &dyn BasicValue, loop_end_bb),
                ];
                variable_val.add_incoming(&phi_incomming);
                let after_bb = self.context.append_basic_block(function, "afterloop");
                self.builder
                    .build_conditional_branch(end_cond, loop_bb, after_bb);
                self.builder.position_at_end(after_bb);
                match variable_old_val {
                    Some(old_val) => s.variables.insert(variable.wrapped.clone(), old_val),
                    None => s.variables.remove(&variable.wrapped),
                };
                Ok((s, self.context.f64_type().const_float(0.0)))
            }
        }
    }
    fn get_insert_block(
        &self,
        expression: &'ctx Expression<X>,
    ) -> Result<BasicBlock<'ctx>, CodeGenError<'ctx, X>> {
        self.builder
            .get_insert_block()
            .ok_or(CodeGenError::InvalidLLVMState(
                expression,
                LLVMStateError::NoInsertBlock,
            ))
    }
    fn get_function(
        &self,
        expression: &'ctx Expression<X>,
    ) -> Result<FunctionValue<'ctx>, CodeGenError<'ctx, X>> {
        self.get_insert_block(expression)?
            .get_parent()
            .ok_or(CodeGenError::InvalidLLVMState(
                expression,
                LLVMStateError::NoParentFunction,
            ))
    }
    fn build_compare(
        &self,
        op: FloatPredicate,
        lhs: FloatValue<'ctx>,
        rhs: FloatValue<'ctx>,
    ) -> FloatValue<'ctx> {
        let int_result = self.builder.build_float_compare(op, lhs, rhs, "cmptmp");
        self.builder
            .build_unsigned_int_to_float(int_result, self.context.f64_type(), "booltmp")
    }
    fn function_prototype(&self, prototype: &'ctx FunctionPrototype<X>) -> FunctionValue<'ctx> {
        let doubles: Vec<BasicMetadataTypeEnum> =
            vec![self.context.f64_type().into(); prototype.args.len()];
        let fn_type = self.context.f64_type().fn_type(&doubles[..], false);
        let function =
            self.module
                .add_function(&prototype.name.wrapped, fn_type, Some(Linkage::External));
        for (i, arg_name) in prototype.args.iter().enumerate() {
            function.get_params()[i].set_name(&arg_name.wrapped);
        }
        function
    }
    fn function(
        &self,
        mut s: MutableState<'ctx>,
        function_dec: &'ctx Function<X>,
    ) -> SResult<'ctx, FunctionValue<'ctx>, CodeGenError<'ctx, X>> {
        let proto = &function_dec.prototype;
        let function = self
            .module
            .get_function(&proto.name.wrapped)
            .unwrap_or_else(|| self.function_prototype(proto));
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);
        s.variables.clear();
        s.variables.reserve(proto.args.len());
        for arg in function.get_param_iter() {
            s.variables
                .insert(name_of_basic_value(&arg), arg.into_float_value());
        }
        let (s, body) = self.expression(s, &function_dec.body)?;
        self.builder.build_return(Some(&body));
        if function.verify(true) {
            self.function_pass_manager.run_on(&function);
            Ok((s, function))
        } else {
            unsafe {
                function.delete();
            }
            Err((s, CodeGenError::InvalidGeneratedFunction(&proto.name)))
        }
    }
}

type SResult<'ctx, O, E> = Result<(MutableState<'ctx>, O), (MutableState<'ctx>, E)>;

fn wrap<'ctx, O, E>(s: MutableState<'ctx>, result: Result<O, E>) -> SResult<'ctx, O, E> {
    match result {
        Ok(ok) => Ok((s, ok)),
        Err(err) => Err((s, err)),
    }
}

fn map<'ctx, O1, O2, E>(
    s: MutableState<'ctx>,
    f: impl FnOnce(O1) -> O2,
    result: Result<O1, E>,
) -> SResult<'ctx, O2, E> {
    match result {
        Ok(ok) => Ok((s, f(ok))),
        Err(err) => Err((s, err)),
    }
}

struct MutableState<'ctx> {
    variables: HashMap<String, FloatValue<'ctx>>,
    main_bb: Option<BasicBlock<'ctx>>,
    main_function: Option<FunctionValue<'ctx>>,
}

impl<'ctx> MutableState<'ctx> {
    fn get_variable(self, name: &str) -> (Self, Option<FloatValue<'ctx>>) {
        let var = self.variables.get(name).map(|var| var.to_owned());
        (self, var)
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
    InvalidLLVMState(&'ctx Expression<X>, LLVMStateError),
}

#[derive(Debug, PartialEq, Eq)]
pub enum LLVMStateError {
    NoInsertBlock,
    NoParentFunction,
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

mod state;
use crate::state::{
    bind, bind_, get_scoped, insert_scoped, map, pure, scoped, try_bind, try_bind_, HasScoped,
    Scoped, ScopedHashMap, State,
};
use kaleidoscope_ast::*;

use std::borrow::Borrow;
use std::borrow::Cow;
use std::ops::Deref;
use std::path::Path;
use std::sync::Arc;

use inkwell::{
    basic_block::BasicBlock,
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
        let mut s = ModuleState::new();
        for ast_value in self.ast_list {
            match ast_value {
                AST::Expression(expr) => {
                    let (expression, st) = self.toplevel_expression(&expr).run_state(s);
                    s = st;
                    self.add_result(expression);
                }
                AST::ExternFunction(prototype) => {
                    self.function_prototype(prototype);
                }
                AST::Function(function) => {
                    let (function, st) = self.function(&function).run_state(s);
                    s = st;
                    self.add_result(function);
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
    fn add_result<T>(&mut self, result: Result<T, CodeGenError<'ctx, X>>) {
        match result {
            Ok(_) => (),
            Err(err) => self.errors.push(err),
        }
    }
    fn toplevel_expression<'a>(
        &'a self,
        expression: &'ctx Expression<X>,
    ) -> impl State<ModuleState<'ctx>, Result<(), CodeGenError<'ctx, X>>> + 'a {
        move |mut s: ModuleState<'ctx>| {
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
            let (expr, mut s) = self.expression(expression).run_state(s);
            let expr = match expr {
                Ok(val) => val,
                Err(err) => return (Err(err), s),
            };
            let bb = match self.get_insert_block(expression) {
                Ok(ok) => ok,
                Err(err) => return (Err(err), s),
            };
            s.main_bb = Some(bb);
            let args = vec![printf_str, expr.into()];
            self.builder.build_call(printf, &args[..], "printtmp");
            (Ok(()), s)
        }
    }
    fn expression<'a>(
        &'a self,
        expression: &'ctx Expression<X>,
    ) -> impl State<ModuleState<'ctx>, Result<FloatValue<'ctx>, CodeGenError<'ctx, X>>> + 'a {
        move |s| match expression.deref() {
            Expr::LiteralNumber(val) => (Ok(self.context.f64_type().const_float(val.wrapped)), s),
            Expr::Variable(name) => map(
                |var| match var {
                    Some(variable) => Ok(variable),
                    None => Err(CodeGenError::VariableNotFound(name.clone())),
                },
                get_scoped(&name.wrapped),
            )
            .run_state(s),
            Expr::BinaryOperation(bin_op) => self.binary_operation(bin_op).run_state(s),
            Expr::Call(Call { callee, args }) => self.function_call(callee, args).run_state(s),
            Expr::If(if_expression) => self.if_expression(if_expression, expression).run_state(s),
            Expr::For(for_expression) => {
                self.for_expression(for_expression, expression).run_state(s)
            }
        }
    }
    fn binary_operation<'a>(
        &'a self,
        bin_op: &'ctx Box<kaleidoscope_ast::BinaryOperation<X>>,
    ) -> impl State<ModuleState<'ctx>, Result<FloatValue<'ctx>, CodeGenError<'ctx, X>>> + 'a {
        bind(self.expression(&bin_op.left_hand_side), move |lhs| {
            map(
                move |rhs| {
                    let lhs = lhs?;
                    let rhs = rhs?;
                    match bin_op.operation.wrapped {
                        Op::Add => Ok(self.builder.build_float_add(lhs, rhs, "addtmp")),
                        Op::Subtract => Ok(self.builder.build_float_sub(lhs, rhs, "subtmp")),
                        Op::Multiply => Ok(self.builder.build_float_mul(lhs, rhs, "multmp")),
                        Op::Divide => Ok(self.builder.build_float_div(lhs, rhs, "divtmp")),
                        Op::Greater => Ok(self.build_compare(FloatPredicate::OGT, lhs, rhs)),
                        Op::GreaterOrEqual => Ok(self.build_compare(FloatPredicate::OGE, lhs, rhs)),
                        Op::Less => Ok(self.build_compare(FloatPredicate::OLT, lhs, rhs)),
                        Op::LessOrEqual => Ok(self.build_compare(FloatPredicate::OLE, lhs, rhs)),
                        Op::Equal => Ok(self.build_compare(FloatPredicate::OEQ, lhs, rhs)),
                        Op::NotEqual => Ok(self.build_compare(FloatPredicate::ONE, lhs, rhs)),
                    }
                },
                self.expression(&bin_op.right_hand_side),
            )
        })
        // let lhs = self.expression(scope, &bin_op.left_hand_side)?;
        // let rhs = self.expression(scope, &bin_op.right_hand_side)?;
    }
    fn function_call<'a>(
        &'a self,
        callee: &'ctx XWrapper<String, X>,
        args: &'ctx Vec<Expression<X>>,
    ) -> impl State<ModuleState<'ctx>, Result<FloatValue<'ctx>, CodeGenError<'ctx, X>>> + 'a {
        move |mut s| -> (_, _) {
            if let Some(callee_function) = self.module.get_function(&callee.wrapped) {
                let cf_arg_len = callee_function.count_params();
                if cf_arg_len != args.len() as u32 {
                    return (
                        Err(CodeGenError::ArgumentLengthMismatch {
                            function_name: callee.clone(),
                            expected: cf_arg_len,
                            actual: args.len(),
                        }),
                        s,
                    );
                }
                let mut args_v: Vec<BasicMetadataValueEnum> = Vec::with_capacity(args.len());
                for arg in args {
                    let (expr, st) = self.expression(&arg).run_state(s);
                    s = st;
                    match expr {
                        Ok(expr) => args_v.push(expr.into()),
                        Err(_) => return (expr, s),
                    };
                }
                (
                    match self
                        .builder
                        .build_call(callee_function, args_v.as_slice(), "calltmp")
                        .try_as_basic_value()
                        .left()
                    {
                        Some(value) => Ok(value.into_float_value()),
                        None => Err(CodeGenError::InvalidFunctionCall(callee.clone())),
                    },
                    s,
                )
            } else {
                (Err(CodeGenError::FunctionNotFound(callee.clone())), s)
            }
        }
    }

    fn if_expression<'a>(
        &'a self,
        if_expression: &'ctx Box<IfExpression<X>>,
        expression: &'ctx Expression<X>,
    ) -> impl State<ModuleState<'ctx>, Result<FloatValue<'ctx>, CodeGenError<'ctx, X>>> + 'a {
        move |s| -> (_, _) {
            let IfExpression {
                condition,
                then,
                r#else,
            } = if_expression.as_ref();
            let (condition_val, s) = self.expression(condition).run_state(s);
            let condition_val = match condition_val {
                Ok(condition_val) => self.builder.build_float_compare(
                    FloatPredicate::ONE,
                    condition_val,
                    self.context.f64_type().const_float(0.0),
                    "ifcond",
                ),
                Err(_) => return (condition_val, s),
            };
            let function = match self.get_function(expression) {
                Ok(function) => function,
                Err(err) => return (Err(err), s),
            };
            let then_bb = self.context.append_basic_block(function, "then");
            let else_bb = self.context.append_basic_block(function, "else");
            let merge_bb = self.context.append_basic_block(function, "merge");
            self.builder
                .build_conditional_branch(condition_val, then_bb, else_bb);
            self.builder.position_at_end(then_bb);
            let (then_val, s) = self.expression(then).run_state(s);
            let then_val = match then_val {
                Ok(val) => val,
                Err(_) => return (then_val, s),
            };
            self.builder.build_unconditional_branch(merge_bb);
            let then_bb = match self.get_insert_block(expression) {
                Ok(val) => val,
                Err(err) => return (Err(err), s),
            };
            self.builder.position_at_end(else_bb);
            let (else_val, s) = self.expression(r#else).run_state(s);
            let else_val = match else_val {
                Ok(val) => val,
                Err(_) => return (else_val, s),
            };
            self.builder.build_unconditional_branch(merge_bb);
            let else_bb = match self.get_insert_block(expression) {
                Ok(val) => val,
                Err(err) => return (Err(err), s),
            };
            self.builder.position_at_end(merge_bb);
            let phi = self.builder.build_phi(self.context.f64_type(), "iftmp");
            let phi_incomming = vec![
                (&then_val as &dyn BasicValue, then_bb),
                (&else_val, else_bb),
            ];
            phi.add_incoming(&phi_incomming);
            (Ok(phi.as_basic_value().into_float_value()), s)
        }
    }

    fn for_expression<'a>(
        &'a self,
        for_expression: &'ctx Box<ForExpression<X>>,
        expression: &'ctx Expression<X>,
    ) -> impl State<ModuleState<'ctx>, Result<FloatValue<'ctx>, CodeGenError<'ctx, X>>> + 'a {
        let ForExpression {
            variable,
            start,
            end,
            step,
            body,
        } = for_expression.as_ref();
        scoped(try_bind(self.expression(start), move |start_val| {
            let function = self.get_function(expression)?;
            let loop_bb = self.context.append_basic_block(function, "loop");
            self.builder.build_unconditional_branch(loop_bb);
            let preheader_bb = self.get_insert_block(expression)?;
            self.builder.position_at_end(loop_bb);
            let variable_val = self
                .builder
                .build_phi(self.context.f64_type(), &variable.wrapped);
            Ok(bind_(
                insert_scoped(
                    &variable.wrapped,
                    variable_val.as_basic_value().into_float_value(),
                ),
                try_bind_(self.expression(body), move |_body_val| {
                    try_bind_(
                        move |s| match step {
                            Some(step) => self.expression(step).run_state(s),
                            None => (Ok(self.context.f64_type().const_float(1.0)), s),
                        },
                        move |step_val| {
                            let next_var = self.builder.build_float_add(
                                variable_val.as_basic_value().into_float_value(),
                                step_val.as_basic_value_enum().into_float_value(),
                                "nextvar",
                            );
                            try_bind(self.expression(end), move |end_cond| {
                                let end_cond = self.builder.build_float_compare(
                                    FloatPredicate::ONE,
                                    end_cond,
                                    self.context.f64_type().const_float(0.0),
                                    "loopcond",
                                );
                                let loop_end_bb = self.get_insert_block(expression)?;
                                let phi_incomming = vec![
                                    (&start_val as &dyn BasicValue, preheader_bb),
                                    (&next_var as &dyn BasicValue, loop_end_bb),
                                ];
                                variable_val.add_incoming(&phi_incomming);
                                let after_bb =
                                    self.context.append_basic_block(function, "afterloop");
                                self.builder
                                    .build_conditional_branch(end_cond, loop_bb, after_bb);
                                self.builder.position_at_end(after_bb);
                                Ok(pure(Ok(self.context.f64_type().const_float(0.0))))
                            })
                        },
                    )
                }),
            ))
        }))
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
    fn function<'a>(
        &'a self,
        function_dec: &'ctx Function<X>,
    ) -> impl State<ModuleState<'ctx>, Result<FunctionValue<'ctx>, CodeGenError<'ctx, X>>> + 'a
    {
        scoped(move |mut s| {
            let proto = &function_dec.prototype;
            let function = self
                .module
                .get_function(&proto.name.wrapped)
                .unwrap_or_else(|| self.function_prototype(proto));
            let entry = self.context.append_basic_block(function, "entry");
            self.builder.position_at_end(entry);
            // scope.reserve(proto.args.len());
            for arg in function.get_param_iter() {
                let name = name_of_basic_value(&arg);
                let name_ref: &str = name.borrow();
                let (_, st) = insert_scoped(name_ref, arg.into_float_value()).run_state(s);
                s = st;
            }
            let (body, s) = self.expression(&function_dec.body).run_state(s);
            let body = match body {
                Ok(val) => val,
                Err(err) => return (Err(err), s),
            };
            self.builder.build_return(Some(&body));
            if function.verify(true) {
                self.function_pass_manager.run_on(&function);
                (Ok(function), s)
            } else {
                unsafe {
                    function.delete();
                }
                (Err(CodeGenError::InvalidGeneratedFunction(&proto.name)), s)
            }
        })
    }
}

struct ModuleState<'ctx> {
    variables: ScopedHashMap<String, FloatValue<'ctx>>,
    main_bb: Option<BasicBlock<'ctx>>,
    main_function: Option<FunctionValue<'ctx>>,
}

impl<'ctx> ModuleState<'ctx> {
    fn new() -> Self {
        ModuleState {
            variables: ScopedHashMap::new(),
            main_bb: None,
            main_function: None,
        }
    }
}

impl<'ctx> HasScoped<ScopedHashMap<String, FloatValue<'ctx>>> for ModuleState<'ctx> {
    fn get_scope<'a>(&'a self) -> &'a ScopedHashMap<String, FloatValue<'ctx>> {
        &self.variables
    }

    fn get_scope_mut<'a>(&'a mut self) -> &'a mut ScopedHashMap<String, FloatValue<'ctx>> {
        &mut self.variables
    }

    fn update(
        self,
        f: impl FnOnce(
            ScopedHashMap<String, FloatValue<'ctx>>,
        ) -> ScopedHashMap<String, FloatValue<'ctx>>,
    ) -> Self {
        ModuleState {
            variables: f(self.variables),
            ..self
        }
    }
}

fn name_of_basic_value<'a>(value: &'a BasicValueEnum) -> Cow<'a, str> {
    match value {
        BasicValueEnum::ArrayValue(val) => val.get_name(),
        BasicValueEnum::IntValue(val) => val.get_name(),
        BasicValueEnum::FloatValue(val) => val.get_name(),
        BasicValueEnum::PointerValue(val) => val.get_name(),
        BasicValueEnum::StructValue(val) => val.get_name(),
        BasicValueEnum::VectorValue(val) => val.get_name(),
    }
    .to_string_lossy()
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

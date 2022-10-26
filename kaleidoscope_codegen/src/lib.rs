/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

mod scope;
#[cfg(test)]
mod tests;

use kaleidoscope_ast::*;
use scope::{scoped, HasScoped, Scoped, ScopedHashMap};

use std::borrow::Borrow;
use std::borrow::Cow;
use std::ops::Deref;
use std::path::Path;
use std::sync::Arc;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::passes::PassManager;
use inkwell::support::LLVMString;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple,
};
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, FloatValue, FunctionValue,
};
use inkwell::{AddressSpace, FloatPredicate, OptimizationLevel};

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
                    let expression = self.toplevel_expression(&mut s, &expr);
                    self.add_result(expression);
                }
                AST::ExternFunction(prototype) => {
                    self.function_prototype(prototype);
                }
                AST::Function(function) => {
                    let function = self.function(&mut s, &function);
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
        s: &mut ModuleState<'ctx>,
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
        let expr = self.expression(s, expression)?;
        let bb = self.get_insert_block(expression)?;
        s.main_bb = Some(bb);
        let args = vec![printf_str, expr.into()];
        self.builder.build_call(printf, &args[..], "printtmp");
        Ok(())
    }
    fn expression<'a>(
        &'a self,
        s: &mut ModuleState<'ctx>,
        expression: &'ctx Expression<X>,
    ) -> Result<FloatValue<'ctx>, CodeGenError<'ctx, X>> {
        match expression.deref() {
            Expr::LiteralNumber(val) => Ok(self.context.f64_type().const_float(val.wrapped)),
            Expr::Variable(name) => match s.get(&name.wrapped) {
                Some(variable) => Ok(variable),
                None => Err(CodeGenError::VariableNotFound(name.clone())),
            },
            Expr::BinaryOperation(bin_op) => self.binary_operation(s, bin_op),
            Expr::Call(Call { callee, args }) => self.function_call(s, callee, args),
            Expr::If(if_expression) => self.if_expression(s, if_expression, expression),
            Expr::For(for_expression) => self.for_expression(s, for_expression, expression),
        }
    }
    fn binary_operation<'a>(
        &'a self,
        s: &mut ModuleState<'ctx>,
        bin_op: &'ctx Box<kaleidoscope_ast::BinaryOperation<X>>,
    ) -> Result<FloatValue<'ctx>, CodeGenError<'ctx, X>> {
        let lhs = self.expression(s, &bin_op.left_hand_side)?;
        let rhs = self.expression(s, &bin_op.right_hand_side)?;
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
    }
    fn function_call<'a>(
        &'a self,
        s: &mut ModuleState<'ctx>,
        callee: &'ctx XWrapper<String, X>,
        args: &'ctx Vec<Expression<X>>,
    ) -> Result<FloatValue<'ctx>, CodeGenError<'ctx, X>> {
        if let Some(callee_function) = self.module.get_function(&callee.wrapped) {
            let cf_arg_len = callee_function.count_params();
            if cf_arg_len != args.len() as u32 {
                return Err(CodeGenError::ArgumentLengthMismatch {
                    function_name: callee.clone(),
                    expected: cf_arg_len,
                    actual: args.len(),
                });
            }
            let mut args_v: Vec<BasicMetadataValueEnum> = Vec::with_capacity(args.len());
            for arg in args {
                let expr = self.expression(s, &arg)?;
                args_v.push(expr.into());
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

    fn if_expression<'a>(
        &'a self,
        s: &mut ModuleState<'ctx>,
        if_expression: &'ctx Box<IfExpression<X>>,
        expression: &'ctx Expression<X>,
    ) -> Result<FloatValue<'ctx>, CodeGenError<'ctx, X>> {
        let IfExpression {
            condition,
            then,
            r#else,
        } = if_expression.as_ref();
        let condition_val = self.expression(s, condition)?;
        let condition_val = self.builder.build_float_compare(
            FloatPredicate::ONE,
            condition_val,
            self.context.f64_type().const_float(0.0),
            "ifcond",
        );
        let function = self.get_function(expression)?;
        let then_bb = self.context.append_basic_block(function, "then");
        let else_bb = self.context.append_basic_block(function, "else");
        let merge_bb = self.context.append_basic_block(function, "merge");
        self.builder
            .build_conditional_branch(condition_val, then_bb, else_bb);
        self.builder.position_at_end(then_bb);
        let then_val = self.expression(s, then)?;
        self.builder.build_unconditional_branch(merge_bb);
        let then_bb = self.get_insert_block(expression)?;
        self.builder.position_at_end(else_bb);
        let else_val = self.expression(s, r#else)?;
        self.builder.build_unconditional_branch(merge_bb);
        let else_bb = self.get_insert_block(expression)?;
        self.builder.position_at_end(merge_bb);
        let phi = self.builder.build_phi(self.context.f64_type(), "iftmp");
        let phi_incomming = vec![
            (&then_val as &dyn BasicValue, then_bb),
            (&else_val, else_bb),
        ];
        phi.add_incoming(&phi_incomming);
        Ok(phi.as_basic_value().into_float_value())
    }

    fn for_expression<'a>(
        &'a self,
        s: &mut ModuleState<'ctx>,
        for_expression: &'ctx Box<ForExpression<X>>,
        expression: &'ctx Expression<X>,
    ) -> Result<FloatValue<'ctx>, CodeGenError<'ctx, X>> {
        scoped(s, move |s| {
            let ForExpression {
                variable,
                start,
                end,
                step,
                body,
            } = for_expression.as_ref();
            let start_val = self.expression(s, start)?;
            let function = self.get_function(expression)?;
            let loop_bb = self.context.append_basic_block(function, "loop");
            self.builder.build_unconditional_branch(loop_bb);
            let preheader_bb = self.get_insert_block(expression)?;
            self.builder.position_at_end(loop_bb);
            let variable_val = self
                .builder
                .build_phi(self.context.f64_type(), &variable.wrapped);
            s.insert(
                &variable.wrapped,
                variable_val.as_basic_value().into_float_value(),
            );
            let _body_val = self.expression(s, body)?;
            let step_val = match step {
                Some(step) => self.expression(s, step)?,
                None => self.context.f64_type().const_float(1.0),
            };
            let next_var = self.builder.build_float_add(
                variable_val.as_basic_value().into_float_value(),
                step_val.as_basic_value_enum().into_float_value(),
                "nextvar",
            );
            let end_cond = self.expression(s, end)?;
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
            let after_bb = self.context.append_basic_block(function, "afterloop");
            self.builder
                .build_conditional_branch(end_cond, loop_bb, after_bb);
            self.builder.position_at_end(after_bb);
            Ok(self.context.f64_type().const_float(0.0))
        })
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
        s: &mut ModuleState<'ctx>,
        function_dec: &'ctx Function<X>,
    ) -> Result<FunctionValue<'ctx>, CodeGenError<'ctx, X>> {
        scoped(s, move |s| {
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
                s.insert(name_ref, arg.into_float_value());
            }
            let body = self.expression(s, &function_dec.body)?;
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

impl<'ctx> HasScoped for ModuleState<'ctx> {
    type Scope = ScopedHashMap<String, FloatValue<'ctx>>;

    fn get_scope(&self) -> &Self::Scope {
        &self.variables
    }

    fn get_scope_mut(&mut self) -> &mut Self::Scope {
        &mut self.variables
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

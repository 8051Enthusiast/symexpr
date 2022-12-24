use std::{collections::HashMap, marker::PhantomData, mem::ManuallyDrop};

use codegen::{
    isa::{CallConv, TargetIsa},
    verify_function,
};
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::Module;

use crate::{
    expr::{self, Arg, Expr, ExprType, VariableValue, Wrap},
    int::Int,
    tuples::{ArgTuple, Project, VarTuple},
};

impl CraneliftValue for () {
    #[inline]
    fn as_cranelift_type() -> Option<Type> {
        None
    }

    #[inline]
    fn as_cranelift_value<'a, B: InstBuilder<'a>>(&self, _: B) -> Option<Value> {
        None
    }
}

impl CraneliftValue for bool {
    #[inline]
    fn as_cranelift_type() -> Option<Type> {
        Some(types::I8)
    }

    #[inline]
    fn as_cranelift_value<'a, B: InstBuilder<'a>>(&self, builder: B) -> Option<Value> {
        Some(builder.iconst(types::I8, *self as i64))
    }
}

impl<I: Int> CraneliftValue for I
where
    i64: From<I>,
{
    #[inline]
    fn as_cranelift_type() -> Option<cranelift::prelude::Type> {
        Some(Type::int(I::BITS).unwrap())
    }

    #[inline]
    fn as_cranelift_value<'a, B: InstBuilder<'a>>(
        &self,
        builder: B,
    ) -> Option<cranelift::prelude::Value> {
        Self::as_cranelift_type().map(|x| builder.iconst(x, i64::from(*self)))
    }
}

impl<C: CraneliftValue + VariableValue> CraneliftVariable for C {}

pub trait CraneliftValue: expr::ExprValue {
    fn as_cranelift_type() -> Option<Type>;
    #[inline]
    fn as_cranelift_argument() -> Option<AbiParam> {
        Self::as_cranelift_type().map(|x| AbiParam::new(x))
    }
    fn as_cranelift_value<'a, B: InstBuilder<'a>>(&self, builder: B) -> Option<Value>;
}

pub trait CraneliftVariable: expr::VariableValue {}

pub trait CraneliftArgs: ArgTuple + Sized {
    type Function<Out>: Fn<Self, Output = Out> + Sized;
    fn as_cranelift_arguments() -> Vec<Option<(AbiParam, ExprType)>>;
    fn as_cranelift_signature(call_conv: isa::CallConv) -> Signature {
        let mut sig = Signature::new(call_conv);
        for arg in Self::as_cranelift_arguments() {
            if let Some(x) = arg {
                sig.params.push(x.0);
            }
        }
        sig
    }
}

impl CraneliftArgs for () {
    type Function<Out> = fn() -> Out;

    fn as_cranelift_arguments() -> Vec<Option<(AbiParam, ExprType)>> {
        vec![]
    }
}

pub trait CraneliftVars: VarTuple {
    fn as_types() -> Vec<ExprType>;
}

pub struct CraneliftFunctionCreator<'a, Sig> {
    pub builder: FunctionBuilder<'a>,
    pub vars: Vec<Option<(Variable, ExprType)>>,
    pub args: Vec<Option<Value>>,
    pub current_types: HashMap<Value, ExprType>,
    pub isa: &'a dyn TargetIsa,
    pub specializations: Option<(&'a Sig, u64)>,
}

impl<'a, Sig> CraneliftFunctionCreator<'a, Sig> {
    pub fn from_fun_builder<'b, Vars: CraneliftVars>(
        mut builder: FunctionBuilder<'b>,
        isa: &'b dyn TargetIsa,
        specializations: Option<(&'b Sig, u64)>,
    ) -> CraneliftFunctionCreator<'b, Sig>
    where
        Sig: CraneliftArgs,
    {
        let varlist = Vars::as_types();
        let mut current_types = HashMap::new();
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let crane_args = Sig::as_cranelift_arguments();
        let mut args = vec![None; crane_args.len()];
        for ((idx, (_, ty)), val) in crane_args
            .into_iter()
            .enumerate()
            .flat_map(|(idx, arg)| arg.map(|x| (idx, x)))
            .zip(builder.block_params(entry_block).iter())
        {
            current_types.insert(*val, ty);
            args[idx] = Some(*val);
        }

        let mut vars = Vec::new();

        for (i, x) in varlist.into_iter().enumerate() {
            if let Some(ty) = x.as_cranelift_type() {
                let var = Variable::new(i);
                builder.declare_var(var, ty);
                vars.push(Some((var, x)));
            } else {
                vars.push(None);
            }
        }
        CraneliftFunctionCreator {
            builder,
            vars,
            args,
            current_types,
            isa,
            specializations,
        }
    }
    pub(crate) fn map_same_typed_value(
        &mut self,
        old_value: Option<Value>,
        mut map: impl FnMut(&mut FunctionBuilder<'a>, Value) -> Value,
    ) -> Option<Value> {
        let old_value = old_value?;
        let old_type = self.current_types[&old_value];
        let new_value = map(&mut self.builder, old_value);
        self.current_types.insert(new_value, old_type);
        Some(new_value)
    }
}

pub trait CraneliftableExpr<Sig> {
    fn cranelift_eval<'a>(&self, ctx: &mut CraneliftFunctionCreator<Sig>) -> Option<Value>;
}

pub trait CraneliftableLExpr<Sig> {
    fn cranelift_write(
        &self,
        ctx: &mut CraneliftFunctionCreator<Sig>,
        val: Option<Value>,
    ) -> Option<Value>;
}

pub struct SpecializationBuilder<Sig, Vars, E>
where
    Sig: CraneliftArgs + CraneliftVars,
    Vars: CraneliftVars,
    E: Expr<Sig, Vars>,
{
    spec: Sig,
    is_set: u64,
    expr: Wrap<Sig, Vars, E>,
}

impl<Sig, Vars, E> SpecializationBuilder<Sig, Vars, E>
where
    E::Output: CraneliftValue,
    Sig: CraneliftArgs + CraneliftVars + Eq + Copy,
    Vars: CraneliftVars,
    E: CraneliftableExpr<Sig> + Expr<Sig, Vars>,
{
    pub(crate) fn from_expr(expr: Wrap<Sig, Vars, E>) -> Self {
        let is_set = 0;
        let spec = Sig::init();
        SpecializationBuilder { spec, is_set, expr }
    }
    pub fn arg<Lens>(mut self, _: Wrap<Sig, Vars, Arg<Sig, Lens::Out, Lens>>, v: Lens::Out) -> Self
    where
        Lens: Project<Sig>,
        Lens::Out: CraneliftValue,
    {
        *Lens::project_mut_ref(&mut self.spec) = v;
        self.is_set |= 1 << Lens::POS;
        self
    }
    pub fn build(
        self,
        jit: &mut CraneliftModule,
    ) -> Wrap<Sig, Vars, SpecializedFunction<'_, Sig, Vars, E>> {
        let is_set = self.is_set.clone();
        let jit_fun = jit
            .create_function(&self.expr.inner, Some((&self.spec, self.is_set)))
            .unwrap();
        let inner = SpecializedFunction {
            jit_fun,
            is_set,
            rust_fun: self.expr,
            spec: self.spec,
            _phantom: PhantomData,
        };
        Wrap::new(inner)
    }
}

#[derive(Clone, Copy)]
pub struct SpecializedFunction<'a, Sig, Vars, E>
where
    Sig: CraneliftArgs,
    Vars: CraneliftVars,
    E: CraneliftableExpr<Sig> + Expr<Sig, Vars>,
{
    spec: Sig,
    is_set: u64,
    jit_fun: Sig::Function<E::Output>,
    rust_fun: Wrap<Sig, Vars, E>,
    _phantom: PhantomData<&'a ()>,
}
impl<'a, Sig, Vars, E> FnOnce<Sig> for SpecializedFunction<'a, Sig, Vars, E>
where
    Sig: Eq,
    Sig: std::marker::Tuple + CraneliftArgs,
    Vars: CraneliftVars,
    E: CraneliftableExpr<Sig> + Expr<Sig, Vars>,
{
    type Output = E::Output;

    extern "rust-call" fn call_once(mut self, args: Sig) -> Self::Output {
        self.call_mut(args)
    }
}

impl<'a, Sig, Vars, E> FnMut<Sig> for SpecializedFunction<'a, Sig, Vars, E>
where
    Sig: Eq,
    Sig: std::marker::Tuple + CraneliftArgs,
    Vars: CraneliftVars,
    E: CraneliftableExpr<Sig> + Expr<Sig, Vars>,
{
    extern "rust-call" fn call_mut(&mut self, args: Sig) -> Self::Output {
        self.call(args)
    }
}

impl<'a, Sig, Vars, E> Fn<Sig> for SpecializedFunction<'a, Sig, Vars, E>
where
    Sig: Eq,
    Sig: std::marker::Tuple + CraneliftArgs,
    Vars: CraneliftVars,
    E: CraneliftableExpr<Sig> + Expr<Sig, Vars>,
{
    extern "rust-call" fn call(&self, args: Sig) -> Self::Output {
        if args.filtered_eq(&self.spec, self.is_set) {
            self.jit_fun.call(args)
        } else {
            self.rust_fun.eval(&args)
        }
    }
}

impl<'a, Sig, Vars, IVars, E> Expr<Sig, IVars> for SpecializedFunction<'a, Sig, Vars, E>
where
    Sig: Eq + Copy,
    Sig: std::marker::Tuple + CraneliftArgs,
    Vars: CraneliftVars,
    E: CraneliftableExpr<Sig> + Expr<Sig, Vars>,
{
    type Output = E::Output;

    fn eval(&self, sig: &Sig, _: &mut IVars) -> Self::Output {
        self.call(*sig)
    }
}

pub struct CraneliftModule {
    functx: FunctionBuilderContext,
    codegen: codegen::Context,
    module: ManuallyDrop<JITModule>,
}

impl CraneliftModule {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn create_function<Sig, Vars, E>(
        &mut self,
        expr: &E,
        specializations: Option<(&Sig, u64)>,
    ) -> Result<Sig::Function<E::Output>, Box<dyn std::error::Error>>
    where
        Sig: CraneliftArgs,
        Vars: CraneliftVars,
        E: Expr<Sig, Vars> + CraneliftableExpr<Sig>,
        E::Output: CraneliftValue,
    {
        self.codegen.clear();
        let fun = &mut self.codegen.func;
        fun.signature = Sig::as_cranelift_signature(CallConv::Fast);
        fun.signature.returns = E::Output::as_cranelift_argument().iter().copied().collect();
        let builder = FunctionBuilder::new(fun, &mut self.functx);
        let mut ctx = CraneliftFunctionCreator::from_fun_builder::<Vars>(
            builder,
            self.module.isa(),
            specializations,
        );
        let ret = expr.cranelift_eval(&mut ctx);
        ctx.builder
            .ins()
            .return_(&ret.into_iter().collect::<Vec<_>>());
        ctx.builder.finalize();
        verify_function(&fun, self.module.isa())?;
        let id = self.module.declare_anonymous_function(&fun.signature)?;
        self.module.define_function(id, &mut self.codegen)?;
        self.module.finalize_definitions()?;
        let fun = self.module.get_finalized_function(id);
        Ok(unsafe { std::mem::transmute_copy::<_, Sig::Function<E::Output>>(&fun) })
    }
}

impl Default for CraneliftModule {
    fn default() -> Self {
        let native_isa =
            cranelift_native::builder().expect("Cannot execute JIT on current platform");
        let mut b = settings::builder();
        b.set("opt_level", "speed")
            .expect("Could not set opimization level");
        b.set("use_egraphs", "true")
            .expect("Could not set use_egraphs");
        let mut optimized_isa_settings = isa::lookup(native_isa.triple().clone())
            .expect("Cannot execute JIT on current platform");
        for sett in native_isa.iter() {
            for value in sett.values.iter().flat_map(|x| x.iter()) {
                optimized_isa_settings.set(sett.name, value).unwrap();
            }
        }
        let optimized_isa = optimized_isa_settings
            .finish(settings::Flags::new(b))
            .unwrap();
        let module = JITModule::new(JITBuilder::with_isa(
            optimized_isa,
            cranelift_module::default_libcall_names(),
        ));
        Self {
            functx: FunctionBuilderContext::new(),
            codegen: module.make_context(),
            module: ManuallyDrop::new(module),
        }
    }
}

impl Drop for CraneliftModule {
    fn drop(&mut self) {
        unsafe {
            let module = ManuallyDrop::take(&mut self.module);
            module.free_memory()
        }
    }
}
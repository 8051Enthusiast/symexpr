pub mod flow;
pub mod ops;

use std::marker::PhantomData;

use cranelift::prelude::{types, Type, Value};

use crate::{
    jit::{
        CraneliftArgs, CraneliftFunctionCreator, CraneliftModule, CraneliftValue, CraneliftVars,
        CraneliftableExpr, CraneliftableLExpr, SpecializationBuilder,
    },
    tuples::{ArgTuple, Project, VarTuple},
};

pub trait AssignExpr<Sig, Vars>: Expr<Sig, Vars>
where
    Self::Output: VariableValue,
{
    fn write(&self, sig: &Sig, vars: &mut Vars, v: Self::Output) -> Self::Output;
}

pub trait ExprValue: Copy {
    fn ty() -> ExprType;
}
pub trait VariableValue: ExprValue + Default {}

pub trait Expr<Signature, Vars> {
    type Output: ExprValue;
    fn eval(&self, sig: &Signature, vars: &mut Vars) -> Self::Output;
}

pub trait BoxableExpr<Out: ExprValue, Sig: CraneliftArgs, Vars>: CraneliftableExpr<Sig> {
    fn eval_gen(&self, sig: &Sig, vars: &mut Vars) -> Out;
}

impl<Sig: CraneliftArgs, Vars, E: Expr<Sig, Vars> + CraneliftableExpr<Sig>>
    BoxableExpr<E::Output, Sig, Vars> for E
{
    #[inline(always)]
    fn eval_gen(&self, sig: &Sig, vars: &mut Vars) -> E::Output {
        self.eval(sig, vars)
    }
}

pub struct BoxedExpr<'a, Out: ExprValue, Sig, Vars> {
    expr: Box<dyn 'a + BoxableExpr<Out, Sig, Vars>>,
    _out: PhantomData<(Out, Sig, Vars)>,
}

impl<'a, Out: ExprValue + 'static, Sig: CraneliftArgs, Vars> Expr<Sig, Vars>
    for BoxedExpr<'a, Out, Sig, Vars>
{
    type Output = Out;

    #[inline(always)]
    fn eval(&self, sig: &Sig, vars: &mut Vars) -> Self::Output {
        self.expr.eval_gen(sig, vars)
    }
}

impl<'a, Out: ExprValue + 'static, Sig: CraneliftArgs, Vars> CraneliftableExpr<Sig>
    for BoxedExpr<'a, Out, Sig, Vars>
{
    fn cranelift_eval(
        &self,
        ctx: &mut CraneliftFunctionCreator<Sig>,
    ) -> Option<cranelift::prelude::Value> {
        self.expr.cranelift_eval(ctx)
    }
}

pub struct Empty(());

impl<Signature, Vars> Expr<Signature, Vars> for Empty {
    type Output = ();

    fn eval(&self, _: &Signature, _: &mut Vars) -> Self::Output {
        ()
    }
}

impl<Sig> CraneliftableExpr<Sig> for Empty {
    #[inline(always)]
    fn cranelift_eval<'a>(
        &self,
        _: &mut CraneliftFunctionCreator<Sig>,
    ) -> Option<cranelift::prelude::Value> {
        None
    }
}

pub type WrapBox<'a, Sig, Vars, Out> = Wrap<Sig, Vars, BoxedExpr<'a, Out, Sig, Vars>>;

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Wrap<Sig, Vars, E: Expr<Sig, Vars>> {
    pub(crate) inner: E,
    _sig: PhantomData<Sig>,
    _vars: PhantomData<Vars>,
}

impl<Sig, Vars, E: Expr<Sig, Vars>> Wrap<Sig, Vars, E> {
    #[inline(always)]
    pub fn eval(&self, sig: &Sig) -> E::Output
    where
        Vars: VarTuple,
    {
        let mut vars = Vars::init();
        self.inner.eval(sig, &mut vars)
    }
    #[inline(always)]
    pub(crate) const fn new(inner: E) -> Self {
        Wrap {
            inner,
            _sig: PhantomData,
            _vars: PhantomData,
        }
    }
    #[inline(always)]
    pub fn boxed<'a>(self) -> Wrap<Sig, Vars, BoxedExpr<'a, E::Output, Sig, Vars>>
    where
        Sig: CraneliftArgs,
        E: 'a + CraneliftableExpr<Sig>,
        E::Output: 'static,
    {
        Wrap::new(BoxedExpr {
            expr: Box::new(self.inner),
            _out: PhantomData,
        })
    }
    pub fn jit(&self, jit: &mut CraneliftModule) -> Sig::Function<E::Output>
    where
        Sig: CraneliftArgs,
        Vars: CraneliftVars,
        E: CraneliftableExpr<Sig>,
        E::Output: CraneliftValue,
    {
        let fun = jit
            .create_function(&self.inner, None)
            .expect("Could not jit");
        unsafe { std::mem::transmute_copy::<_, Sig::Function<E::Output>>(&fun) }
    }
    pub fn with_specialization(self) -> SpecializationBuilder<Sig, Vars, E>
    where
        Sig: CraneliftVars + CraneliftArgs,
        Vars: CraneliftVars,
        E: CraneliftableExpr<Sig>,
        E::Output: CraneliftValue,
    {
        SpecializationBuilder::from_expr(self)
    }
}

impl<V: ExprValue, Sig, Vars> From<V> for Wrap<Sig, Vars, Val<V>> {
    #[inline(always)]
    fn from(i: V) -> Self {
        Wrap::new(Val { val: i })
    }
}

impl<Sig: std::marker::Tuple, Vars: VarTuple, E: Expr<Sig, Vars>> FnOnce<Sig>
    for Wrap<Sig, Vars, E>
{
    type Output = E::Output;

    #[inline(always)]
    extern "rust-call" fn call_once(self, args: Sig) -> Self::Output {
        self.eval(&args)
    }
}
impl<Sig: std::marker::Tuple, Vars: VarTuple, E: Expr<Sig, Vars>> FnMut<Sig>
    for Wrap<Sig, Vars, E>
{
    #[inline(always)]
    extern "rust-call" fn call_mut(&mut self, args: Sig) -> Self::Output {
        self.eval(&args)
    }
}
impl<Sig: std::marker::Tuple, Vars: VarTuple, E: Expr<Sig, Vars>> Fn<Sig> for Wrap<Sig, Vars, E> {
    #[inline(always)]
    extern "rust-call" fn call(&self, args: Sig) -> Self::Output {
        self.eval(&args)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ExprType {
    Unit,
    Bool,
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    ISize,
    USize,
    I128,
    U128,
    Slice,
}
impl ExprType {
    pub fn as_cranelift_type(self) -> Option<Type> {
        Some(match self {
            ExprType::Unit => return None,
            ExprType::Bool => types::B1,
            ExprType::I8 | ExprType::U8 => types::I8,
            ExprType::I16 | ExprType::U16 => types::I16,
            ExprType::I32 | ExprType::U32 => types::I32,
            ExprType::I64 | ExprType::U64 | ExprType::ISize | ExprType::USize => types::I64,
            ExprType::I128 | ExprType::U128 => types::I128,
            ExprType::Slice => types::I128,
        })
    }
    pub fn is_signed(&self) -> bool {
        match self {
            ExprType::Unit
            | ExprType::Bool
            | ExprType::U8
            | ExprType::U16
            | ExprType::U32
            | ExprType::U64
            | ExprType::USize
            | ExprType::U128
            | ExprType::Slice => false,
            ExprType::I8
            | ExprType::I16
            | ExprType::I32
            | ExprType::I64
            | ExprType::ISize
            | ExprType::I128 => true,
        }
    }
}

impl ExprValue for () {
    fn ty() -> ExprType {
        ExprType::Unit
    }
}
impl ExprValue for bool {
    fn ty() -> ExprType {
        ExprType::Bool
    }
}
impl<C: ExprValue> ExprValue for &C {
    fn ty() -> ExprType {
        ExprType::I64
    }
}
impl<C: ExprValue> ExprValue for &[C] {
    fn ty() -> ExprType {
        ExprType::Slice
    }
}
impl<C: ExprValue + Default> VariableValue for C {}

#[derive(Clone, Copy)]
pub struct Arg<Sig, Out, L>
where
    Out: ExprValue,
    L: Project<Sig, Out = Out>,
{
    pub(crate) _l: L,
    pub(crate) _sig: PhantomData<Sig>,
}

impl<Sig, Vars, Out, L> Expr<Sig, Vars> for Arg<Sig, Out, L>
where
    Out: ExprValue,
    L: Project<Sig, Out = Out>,
{
    type Output = Out;

    #[inline(always)]
    fn eval(&self, sig: &Sig, _: &mut Vars) -> Self::Output {
        *L::project_ref(sig)
    }
}

impl<Sig, Out, L> CraneliftableExpr<Sig> for Arg<Sig, Out, L>
where
    Out: CraneliftValue,
    Sig: CraneliftVars,
    L: Project<Sig, Out = Out>,
{
    fn cranelift_eval<'a>(
        &self,
        ctx: &mut CraneliftFunctionCreator<Sig>,
    ) -> Option<cranelift::prelude::Value> {
        if let Some((x, _)) = ctx
            .specializations
            .as_ref()
            .filter(|(_, is_set)| is_set[L::POS])
        {
            let val = L::project_ref(x).as_cranelift_value(ctx.builder.ins())?;
            let ty = L::Out::ty();
            ctx.current_types.insert(val, ty);
            Some(val)
        } else {
            ctx.args[L::POS]
        }
    }
}

pub fn v<Sig, Vars, V: ExprValue>(val: V) -> Wrap<Sig, Vars, Val<V>> {
    Wrap {
        inner: Val { val },
        _sig: PhantomData,
        _vars: PhantomData,
    }
}

#[derive(Clone, Copy)]
pub struct Val<V: ExprValue> {
    val: V,
}

impl<Sig, Vars, V: ExprValue> Expr<Sig, Vars> for Val<V> {
    type Output = V;

    #[inline(always)]
    fn eval(&self, _: &Sig, _: &mut Vars) -> Self::Output {
        self.val
    }
}

impl<V: CraneliftValue, Sig> CraneliftableExpr<Sig> for Val<V> {
    fn cranelift_eval<'a>(
        &self,
        ctx: &mut CraneliftFunctionCreator<Sig>,
    ) -> Option<cranelift::prelude::Value> {
        let v = self.val.as_cranelift_value(ctx.builder.ins())?;
        ctx.current_types.insert(v, V::ty());
        Some(v)
    }
}

#[derive(Clone, Copy)]
pub struct Var<Vars, Out, L>
where
    Out: VariableValue,
    L: Project<Vars, Out = Out>,
{
    pub(crate) _l: L,
    pub(crate) _sig: PhantomData<Vars>,
}

impl<Sig, Vars, Out, L> Expr<Sig, Vars> for Var<Vars, Out, L>
where
    Vars: VarTuple,
    Sig: ArgTuple,
    Out: VariableValue,
    L: Project<Vars, Out = Out>,
{
    type Output = Out;

    #[inline(always)]
    fn eval(&self, _: &Sig, vars: &mut Vars) -> Self::Output {
        *L::project_ref(vars)
    }
}

impl<Sig, Vars, Out, L> CraneliftableExpr<Sig> for Var<Vars, Out, L>
where
    Out: VariableValue,
    L: Project<Vars, Out = Out>,
{
    fn cranelift_eval<'a>(
        &self,
        ctx: &mut CraneliftFunctionCreator<Sig>,
    ) -> Option<cranelift::prelude::Value> {
        let (var, ty) = ctx.vars[L::POS]?;
        let val = ctx.builder.use_var(var);
        ctx.current_types.insert(val, ty);
        Some(val)
    }
}

impl<Sig, Vars, Out, L> AssignExpr<Sig, Vars> for Var<Vars, Out, L>
where
    Vars: VarTuple,
    Sig: ArgTuple,
    Out: VariableValue,
    L: Project<Vars, Out = Out>,
{
    #[inline(always)]
    fn write(&self, _: &Sig, vars: &mut Vars, v: Out) -> Out {
        *L::project_mut_ref(vars) = v;
        v
    }
}

impl<Sig, Vars, Out, L> CraneliftableLExpr<Sig> for Var<Vars, Out, L>
where
    Out: VariableValue,
    L: Project<Vars, Out = Out>,
{
    fn cranelift_write<'a>(
        &self,
        ctx: &mut CraneliftFunctionCreator<Sig>,
        val: Option<Value>,
    ) -> Option<Value> {
        let val = val?;
        let var = ctx.vars[L::POS]?;
        ctx.builder.def_var(var.0, val);
        Some(val)
    }
}

pub mod prelude {
    use crate::tuples::{ArgTuple, VarTuple};

    pub use super::v;
    pub use super::WrapBox;
    use super::*;
    pub use flow::{when, whilst};
    pub use ops::{eq, ge, gt, le, lt, ne};
    #[inline(always)]
    pub fn args<Sig: ArgTuple, Vars: VarTuple>() -> Sig::Args<Vars> {
        Sig::args::<Vars>()
    }
    #[inline(always)]
    pub fn vars<Vars: VarTuple, Sig: ArgTuple>() -> Vars::Vars<Sig> {
        Vars::vars::<Sig>()
    }
}

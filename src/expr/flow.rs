use cranelift::prelude::{InstBuilder};

use crate::jit::{CraneliftableExpr, CraneliftableLExpr, CraneliftFunctionCreator};

use super::{AssignExpr, Empty, Expr, VariableValue, Wrap};

#[derive(Clone, Copy)]
pub struct ExprSeq<E, F> {
    left: E,
    right: F,
}

impl<Sig, Vars, E: Expr<Sig, Vars>, F: Expr<Sig, Vars>> Expr<Sig, Vars> for ExprSeq<E, F> {
    type Output = F::Output;

    #[inline(always)]
    fn eval(&self, sig: &Sig, vars: &mut Vars) -> Self::Output {
        self.left.eval(sig, vars);
        self.right.eval(sig, vars)
    }
}

impl<Sig, E: CraneliftableExpr<Sig>, F: CraneliftableExpr<Sig>> CraneliftableExpr<Sig> for ExprSeq<E, F> {
    fn cranelift_eval<'a>(&self, ctx: &mut CraneliftFunctionCreator<Sig>) -> Option<cranelift::prelude::Value> {
        self.left.cranelift_eval(ctx);
        self.right.cranelift_eval(ctx)
    }
}

impl<Sig, Vars, E: Expr<Sig, Vars>> Wrap<Sig, Vars, E> {
    #[inline(always)]
    pub fn then<F: Expr<Sig, Vars>>(
        self,
        right: Wrap<Sig, Vars, F>,
    ) -> Wrap<Sig, Vars, ExprSeq<E, F>> {
        Wrap::new(ExprSeq {
            left: self.inner,
            right: right.inner,
        })
    }
}

#[derive(Clone, Copy)]
pub struct ExprIf<E, F> {
    cond: E,
    body: F,
}

impl<Sig, Vars, E: Expr<Sig, Vars, Output = bool>, F: Expr<Sig, Vars>> Expr<Sig, Vars>
    for ExprIf<E, F>
{
    type Output = ();

    #[inline(always)]
    fn eval(&self, sig: &Sig, vars: &mut Vars) -> Self::Output {
        if self.cond.eval(sig, vars) {
            self.body.eval(sig, vars);
        }
    }
}
impl<Sig, E: CraneliftableExpr<Sig>, F: CraneliftableExpr<Sig>> CraneliftableExpr<Sig> for ExprIf<E, F> {
    fn cranelift_eval<'a>(&self, ctx: &mut CraneliftFunctionCreator<Sig>) -> Option<cranelift::prelude::Value> {
        let cond = self.cond.cranelift_eval(ctx).unwrap();
        let cond_block = ctx.builder.create_block();
        let after_block = ctx.builder.create_block();
        ctx.builder.ins().brnz(cond, cond_block, &[]);
        ctx.builder.seal_block(cond_block);
        ctx.builder.ins().jump(after_block, &[]);
        ctx.builder.switch_to_block(cond_block);
        self.body.cranelift_eval(ctx);
        ctx.builder.ins().jump(after_block, &[]);
        ctx.builder.seal_block(after_block);
        ctx.builder.switch_to_block(after_block);
        None
    }
}

#[inline(always)]
pub fn when<Sig, Vars, E: Expr<Sig, Vars, Output = bool>>(
    cond: impl Into<Wrap<Sig, Vars, E>>,
) -> Wrap<Sig, Vars, ExprIf<E, Empty>> {
    Wrap::new(ExprIf {
        cond: cond.into().inner,
        body: super::Empty(()),
    })
}

impl<Sig, Vars, E: Expr<Sig, Vars, Output = bool>> Wrap<Sig, Vars, ExprIf<E, Empty>> {
    #[inline(always)]
    pub fn doth<F: Expr<Sig, Vars>>(
        self,
        body: impl Into<Wrap<Sig, Vars, F>>,
    ) -> Wrap<Sig, Vars, ExprIf<E, F>> {
        Wrap::new(ExprIf {
            cond: self.inner.cond,
            body: body.into().inner,
        })
    }
}

#[derive(Clone, Copy)]
pub struct ExprIfElse<E, F, G> {
    cond: E,
    then: F,
    otherwise: G,
}

impl<Sig, Vars, E, F, G> Expr<Sig, Vars> for ExprIfElse<E, F, G>
where
    E: Expr<Sig, Vars, Output = bool>,
    F: Expr<Sig, Vars>,
    G: Expr<Sig, Vars, Output = F::Output>,
{
    type Output = F::Output;

    #[inline(always)]
    fn eval(&self, sig: &Sig, vars: &mut Vars) -> Self::Output {
        if self.cond.eval(sig, vars) {
            self.then.eval(sig, vars)
        } else {
            self.otherwise.eval(sig, vars)
        }
    }
}

impl<Sig, E: CraneliftableExpr<Sig>, F: CraneliftableExpr<Sig>, G: CraneliftableExpr<Sig>> CraneliftableExpr<Sig>
    for ExprIfElse<E, F, G>
{
    fn cranelift_eval<'a>(&self, ctx: &mut CraneliftFunctionCreator<Sig>) -> Option<cranelift::prelude::Value> {
        let cond = self.cond.cranelift_eval(ctx).unwrap();

        let then_block = ctx.builder.create_block();
        let otherwise_block = ctx.builder.create_block();
        ctx.builder.ins().brnz(cond, then_block, &[]);
        ctx.builder.seal_block(then_block);
        ctx.builder.ins().jump(otherwise_block, &[]);
        ctx.builder.seal_block(otherwise_block);
        ctx.builder.switch_to_block(then_block);
        let then_val = self.then.cranelift_eval(ctx);
        let after_block = ctx.builder.create_block();
        let ret = if let Some(x) = then_val {
            let ret_type = *ctx.current_types.get(&x).unwrap();
            let ret_val = ctx.builder.append_block_param(after_block, ret_type.as_cranelift_type().unwrap());
            ctx.current_types.insert(ret_val, ret_type);
            ctx.builder.ins().jump(after_block, &[x]);
            Some(ret_val)
        } else {
            ctx.builder.ins().jump(after_block, &[]);
            None
        };
        ctx.builder.switch_to_block(otherwise_block);
        let otherwise_val = self.otherwise.cranelift_eval(ctx);
        if let Some(x) = otherwise_val {
            ctx.builder.ins().jump(after_block, &[x]);
        } else {
            ctx.builder.ins().jump(after_block, &[]);
        }
        ctx.builder.seal_block(after_block);
        ctx.builder.switch_to_block(after_block);
        ret
    }
}

impl<Sig, Vars, E: Expr<Sig, Vars, Output = bool>, F: Expr<Sig, Vars>>
    Wrap<Sig, Vars, ExprIf<E, F>>
{
    #[inline(always)]
    pub fn otherwise<G: Expr<Sig, Vars, Output = F::Output>>(
        self,
        otherwise: impl Into<Wrap<Sig, Vars, G>>,
    ) -> Wrap<Sig, Vars, ExprIfElse<E, F, G>> {
        Wrap::new(ExprIfElse {
            cond: self.inner.cond,
            then: self.inner.body,
            otherwise: otherwise.into().inner,
        })
    }
}

#[derive(Clone, Copy)]
pub struct ExprWhile<E, F> {
    cond: E,
    body: F,
}

impl<Sig, Vars, E: Expr<Sig, Vars, Output = bool>, F: Expr<Sig, Vars>> Expr<Sig, Vars>
    for ExprWhile<E, F>
{
    type Output = ();

    #[inline(always)]
    fn eval(&self, sig: &Sig, vars: &mut Vars) -> Self::Output {
        while self.cond.eval(sig, vars) {
            self.body.eval(sig, vars);
        }
    }
}

impl<Sig, E: CraneliftableExpr<Sig>, F: CraneliftableExpr<Sig>> CraneliftableExpr<Sig> for ExprWhile<E, F> {
    fn cranelift_eval<'a>(&self, ctx: &mut CraneliftFunctionCreator<Sig>) -> Option<cranelift::prelude::Value> {
        let cond_block = ctx.builder.create_block();
        ctx.builder.ins().jump(cond_block, &[]);
        ctx.builder.switch_to_block(cond_block);
        let cond = self.cond.cranelift_eval(ctx).unwrap();
        let after_block = ctx.builder.create_block();
        let body_block = ctx.builder.create_block();
        ctx.builder.ins().brnz(cond, body_block, &[]);
        ctx.builder.seal_block(body_block);
        ctx.builder.ins().jump(after_block, &[]);
        ctx.builder.seal_block(after_block);
        ctx.builder.switch_to_block(body_block);
        self.body.cranelift_eval(ctx);
        ctx.builder.ins().jump(cond_block, &[]);
        ctx.builder.seal_block(cond_block);
        ctx.builder.switch_to_block(after_block);
        None
    }
}

#[inline(always)]
pub fn whilst<Sig, Vars, E: Expr<Sig, Vars, Output = bool>>(
    cond: impl Into<Wrap<Sig, Vars, E>>,
) -> Wrap<Sig, Vars, ExprWhile<E, Empty>> {
    Wrap::new(ExprWhile {
        cond: cond.into().inner,
        body: super::Empty(()),
    })
}

impl<Sig, Vars, E: Expr<Sig, Vars, Output = bool>> Wrap<Sig, Vars, ExprWhile<E, Empty>> {
    #[inline(always)]
    pub fn doth<F: Expr<Sig, Vars>>(
        self,
        body: impl Into<Wrap<Sig, Vars, F>>,
    ) -> Wrap<Sig, Vars, ExprWhile<E, F>> {
        Wrap::new(ExprWhile {
            cond: self.inner.cond,
            body: body.into().inner,
        })
    }
}

#[derive(Clone, Copy)]
pub struct ExprAssign<E, F> {
    var: E,
    expr: F,
}

impl<Sig, Vars, E: AssignExpr<Sig, Vars>, F: Expr<Sig, Vars, Output = E::Output>> Expr<Sig, Vars>
    for ExprAssign<E, F>
where
    E::Output: VariableValue,
{
    type Output = E::Output;

    #[inline(always)]
    fn eval(&self, sig: &Sig, vars: &mut Vars) -> Self::Output {
        let res = self.expr.eval(sig, vars);
        self.var.write(sig, vars, res)
    }
}

impl<Sig, E: CraneliftableLExpr<Sig>, F: CraneliftableExpr<Sig>> CraneliftableExpr<Sig> for ExprAssign<E, F> {
    fn cranelift_eval<'a>(&self, ctx: &mut CraneliftFunctionCreator<Sig>) -> Option<cranelift::prelude::Value> {
        let val = self.expr.cranelift_eval(ctx);
        self.var.cranelift_write(ctx, val)
    }
}

impl<Sig, Vars, E: AssignExpr<Sig, Vars>> Wrap<Sig, Vars, E>
where
    E::Output: VariableValue,
{
    #[inline(always)]
    pub fn set<F: Expr<Sig, Vars, Output = E::Output>>(
        self,
        expr: impl Into<Wrap<Sig, Vars, F>>,
    ) -> Wrap<Sig, Vars, ExprAssign<E, F>> {
        Wrap::new(ExprAssign {
            var: self.inner,
            expr: expr.into().inner,
        })
    }
}

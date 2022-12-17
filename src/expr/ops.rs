use std::ops::*;

use cranelift::prelude::{InstBuilder, IntCC, types};

use crate::{
    expr::{Expr, ExprType, Wrap},
    int::{Int, UsualArithmeticConverted},
    jit::{CraneliftableExpr, CraneliftFunctionCreator},
};

macro_rules! impl_arithmetic {
    ($name:ident, $trait:ident, $funname:ident $(, $insname:ident)?) => {
        #[derive(Clone, Copy)]
        pub struct $name<E, F> {
            left: E,
            right: F,
        }

        impl<Sig, Vars, E, F> std::ops::$trait<Wrap<Sig, Vars, F>> for Wrap<Sig, Vars, E>
        where
            E: Expr<Sig, Vars>,
            F: Expr<Sig, Vars>,
            E::Output: UsualArithmeticConverted<F::Output>,
            F::Output: Int,
        {
            type Output = Wrap<Sig, Vars, $name<E, F>>;

            #[inline(always)]
            fn $funname(self, rhs: Wrap<Sig, Vars, F>) -> Self::Output {
                Wrap::new($name {
                    left: self.inner,
                    right: rhs.inner,
                })
            }
        }

        impl<Sig, Vars, E, I> std::ops::$trait<I> for Wrap<Sig, Vars, E>
        where
            E: Expr<Sig, Vars>,
            I: Int,
            E::Output: UsualArithmeticConverted<I>,
        {
            type Output = Wrap<Sig, Vars, $name<E, super::Val<I>>>;

            #[inline(always)]
            fn $funname(self, rhs: I) -> Self::Output {
                Wrap::new($name {
                    left: self.inner,
                    right: super::Val { val: rhs },
                })
            }
        }

        impl<Sig, Vars, E, F> Expr<Sig, Vars> for $name<E, F>
        where
            E: Expr<Sig, Vars>,
            F: Expr<Sig, Vars>,
            E::Output: UsualArithmeticConverted<F::Output>,
            F::Output: Int,
        {
            type Output = <E::Output as UsualArithmeticConverted<F::Output>>::Result;

            #[inline(always)]
            fn eval(&self, sig: &Sig, vars: &mut Vars) -> Self::Output {
                let lhs = self.left.eval(sig, vars);
                let rhs = self.right.eval(sig, vars);
                let (lhs, rhs) = lhs.usual_convert(rhs);
                lhs.$funname(rhs)
            }
        }

        $(
        impl<Sig, E: CraneliftableExpr<Sig>, F: CraneliftableExpr<Sig>> CraneliftableExpr<Sig> for $name<E, F> {
            fn cranelift_eval<'a>(
                &self,
                ctx: &mut crate::jit::CraneliftFunctionCreator<Sig>,
            ) -> Option<cranelift::prelude::Value> {
                let lhs = self.left.cranelift_eval(ctx);
                let rhs = self.right.cranelift_eval(ctx);
                ctx.map_same_typed_value(lhs, |builder, v| builder.ins().$insname(v, rhs.unwrap()))
            }
        }
        )?
    };

}

macro_rules! impl_signed_op {
    ($name:ident, $signed:ident, $unsigned:ident) => {
        impl<Sig, E: CraneliftableExpr<Sig>, F: CraneliftableExpr<Sig>> CraneliftableExpr<Sig> for $name<E, F> {
            fn cranelift_eval<'a>(
                &self,
                ctx: &mut crate::jit::CraneliftFunctionCreator<Sig>,
            ) -> Option<cranelift::prelude::Value> {
                let lhs = self.left.cranelift_eval(ctx);
                let rhs = self.right.cranelift_eval(ctx)?;
                if ctx.current_types[&rhs].is_signed() {
                    ctx.map_same_typed_value(lhs, |builder, v| builder.ins().$signed(v, rhs))
                } else {
                    ctx.map_same_typed_value(lhs, |builder, v| builder.ins().$unsigned(v, rhs))
                }
            }
        }
    };
}

impl_arithmetic!(ExprAdd, Add, add, iadd);
impl_arithmetic!(ExprMul, Mul, mul, imul);
impl_arithmetic!(ExprSub, Sub, sub, isub);
impl_arithmetic!(ExprBitOr, BitOr, bitor, bor);
impl_arithmetic!(ExprBitXor, BitXor, bitxor, bxor);
impl_arithmetic!(ExprBitAnd, BitAnd, bitand, band);
impl_arithmetic!(ExprShl, Shl, shl, ishl);
impl_arithmetic!(ExprShr, Shr, shr);
impl_signed_op!(ExprShr, sshr, ushr);
impl_arithmetic!(ExprRem, Rem, rem);
impl_signed_op!(ExprRem, srem, urem);
impl_arithmetic!(ExprDiv, Div, div);
impl_signed_op!(ExprDiv, sdiv, udiv);

macro_rules! impl_arithmetic_unary {
    ($name:ident, $trait:ident, $funname:ident, $insname:ident) => {
        #[derive(Clone, Copy)]
        pub struct $name<E>(E);

        impl<Sig, Vars, E> std::ops::$trait for Wrap<Sig, Vars, E>
        where
            E: Expr<Sig, Vars>,
            E::Output: $trait,
            <E::Output as $trait>::Output: super::ExprValue,
        {
            type Output = Wrap<Sig, Vars, $name<E>>;

            #[inline(always)]
            fn $funname(self) -> Self::Output {
                Wrap::new($name(self.inner))
            }
        }

        impl<Sig, Vars, E> Expr<Sig, Vars> for $name<E>
        where
            E: Expr<Sig, Vars>,
            E::Output: $trait,
            <E::Output as $trait>::Output: super::ExprValue,
        {
            type Output = <E::Output as $trait>::Output;

            #[inline(always)]
            fn eval(&self, sig: &Sig, vars: &mut Vars) -> Self::Output {
                self.0.eval(sig, vars).$funname()
            }
        }
        impl<Sig, E: CraneliftableExpr<Sig>> CraneliftableExpr<Sig> for $name<E> {
            fn cranelift_eval<'a>(
                &self,
                ctx: &mut crate::jit::CraneliftFunctionCreator<Sig>,
            ) -> Option<cranelift::prelude::Value> {
                let v = self.0.cranelift_eval(ctx);
                ctx.map_same_typed_value(v, |builder, v| builder.ins().$insname(v))
            }
        }
    };
}

impl_arithmetic_unary!(ExprNeg, Neg, neg, ineg);
impl_arithmetic_unary!(ExprNot, Not, not, bnot);

macro_rules! impl_cmp {
    ($name:ident, $trait:ident, $funname:ident) => {
        #[derive(Clone, Copy)]
        pub struct $name<E, F> {
            left: E,
            right: F,
        }

        #[inline(always)]
        pub fn $funname<Sig, Vars, E, F>(
            left: impl Into<Wrap<Sig, Vars, E>>,
            right: impl Into<Wrap<Sig, Vars, F>>,
        ) -> Wrap<Sig, Vars, $name<E, F>>
        where
            E::Output: $trait,
            E: Expr<Sig, Vars>,
            F: Expr<Sig, Vars, Output = E::Output>,
        {
            Wrap::new($name {
                left: left.into().inner,
                right: right.into().inner,
            })
        }
        impl<Sig, Vars, E: Expr<Sig, Vars>> Wrap<Sig, Vars, E> {
            #[inline(always)]
            pub fn $funname<F: Expr<Sig, Vars, Output = E::Output>>(
                self,
                right: impl Into<Wrap<Sig, Vars, F>>,
            ) -> Wrap<Sig, Vars, $name<E, F>>
            where
                E::Output: $trait<E::Output>,
            {
                Wrap::new($name {
                    left: self.inner,
                    right: right.into().inner,
                })
            }
        }

        impl<Sig, Vars, E, F> Expr<Sig, Vars> for $name<E, F>
        where
            E::Output: $trait<E::Output>,
            E: Expr<Sig, Vars>,
            F: Expr<Sig, Vars, Output = E::Output>,
        {
            type Output = bool;

            #[inline(always)]
            fn eval(&self, sig: &Sig, vars: &mut Vars) -> Self::Output {
                let lhs = self.left.eval(sig, vars);
                let rhs = self.right.eval(sig, vars);
                lhs.$funname(&rhs)
            }
        }
    };
}

macro_rules! impl_cmp_op {
    (unsigned: $name:ident, $op:path) => {
        impl<Sig, E: CraneliftableExpr<Sig>, F: CraneliftableExpr<Sig>> CraneliftableExpr<Sig> for $name<E, F> {
            fn cranelift_eval<'a>(
                &self,
                ctx: &mut crate::jit::CraneliftFunctionCreator<Sig>,
            ) -> Option<cranelift::prelude::Value> {
                let lhs = self.left.cranelift_eval(ctx)?;
                let rhs = self.right.cranelift_eval(ctx)?;
                let ret = ctx.builder.ins().icmp($op, lhs, rhs);
                ctx.current_types.insert(ret, ExprType::Bool);
                Some(ret)
            }
        }
    };
    (signed: $name:ident, $sop:path, $uop:path) => {
        impl<Sig, E: CraneliftableExpr<Sig>, F: CraneliftableExpr<Sig>> CraneliftableExpr<Sig> for $name<E, F> {
            fn cranelift_eval<'a>(
                &self,
                ctx: &mut crate::jit::CraneliftFunctionCreator<Sig>,
            ) -> Option<cranelift::prelude::Value> {
                let lhs = self.left.cranelift_eval(ctx)?;
                let rhs = self.right.cranelift_eval(ctx)?;
                let ret = if ctx.current_types[&rhs].is_signed() {
                    ctx.builder.ins().icmp($sop, lhs, rhs)
                } else {
                    ctx.builder.ins().icmp($uop, lhs, rhs)
                };
                ctx.current_types.insert(ret, ExprType::Bool);
                Some(ret)
            }
        }
    };
}

impl_cmp!(ExprEq, PartialEq, eq);
impl_cmp_op!(unsigned: ExprEq, IntCC::Equal);
impl_cmp!(ExprNeq, PartialEq, ne);
impl_cmp_op!(unsigned: ExprNeq, IntCC::NotEqual);
impl_cmp!(ExprLt, PartialOrd, lt);
impl_cmp_op!(
    signed: ExprLt,
    IntCC::SignedLessThan,
    IntCC::UnsignedLessThan
);
impl_cmp!(ExprGt, PartialOrd, gt);
impl_cmp_op!(
    signed: ExprGt,
    IntCC::SignedGreaterThan,
    IntCC::UnsignedGreaterThan
);
impl_cmp!(ExprLe, PartialOrd, le);
impl_cmp_op!(
    signed: ExprLe,
    IntCC::SignedLessThanOrEqual,
    IntCC::UnsignedLessThanOrEqual
);
impl_cmp!(ExprGe, PartialOrd, ge);
impl_cmp_op!(
    signed: ExprGe,
    IntCC::SignedGreaterThanOrEqual,
    IntCC::UnsignedGreaterThanOrEqual
);

macro_rules! impl_bool_op {
    ($name:ident, $funname:ident, $op:tt, $jop:ident) => {
        #[derive(Clone, Copy)]
        pub struct $name<E, F> {
            left: E,
            right: F,
        }

        impl<Sig, Vars, E, F> Expr<Sig, Vars> for $name<E, F>
        where
            E: Expr<Sig, Vars, Output = bool>,
            F: Expr<Sig, Vars, Output = bool>,
        {
            type Output = bool;

            #[inline(always)]
            fn eval(&self, sig: &Sig, vars: &mut Vars) -> Self::Output {
                self.left.eval(sig, vars) $op self.right.eval(sig, vars)
            }
        }

        impl<Sig, Vars, E: Expr<Sig, Vars, Output = bool>> Wrap<Sig, Vars, E> {
            #[inline(always)]
            pub fn $funname<F: Expr<Sig, Vars, Output = bool>>(
                self,
                right: impl Into<Wrap<Sig, Vars, F>>,
            ) -> Wrap<Sig, Vars, $name<E, F>>
            {
                Wrap::new($name {
                    left: self.inner,
                    right: right.into().inner,
                })
            }
        }
        impl<Sig, E: CraneliftableExpr<Sig>, F: CraneliftableExpr<Sig>> CraneliftableExpr<Sig> for $name<E, F> {
            fn cranelift_eval<'a>(&self, ctx: &mut CraneliftFunctionCreator<Sig>) -> Option<cranelift::prelude::Value> {
                let left = self.left.cranelift_eval(ctx).unwrap();
                let right_block = ctx.builder.create_block();
                ctx.builder.ins().$jop(left, right_block, &[]);
                ctx.builder.seal_block(right_block);
            
                let after_block = ctx.builder.create_block();
                let ret = ctx.builder.append_block_param(after_block, types::B1);
                ctx.builder.ins().jump(after_block, &[left]);
                ctx.builder.switch_to_block(right_block);
                let right = self.right.cranelift_eval(ctx).unwrap();
                ctx.builder.ins().jump(after_block, &[right]);
                ctx.builder.seal_block(after_block);
                ctx.builder.switch_to_block(after_block);
                Some(ret)
            }
        }
    }
}

impl_bool_op!(ExprAnd, and, &&, brnz);
impl_bool_op!(ExprOr, or, ||, brz);
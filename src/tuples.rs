use std::marker::PhantomData;

use cranelift::prelude::*;

use crate::expr::{VariableValue, Arg, Var, Wrap, ExprType};
use crate::jit::{CraneliftArgs, CraneliftVars};
use crate::jit::CraneliftValue;

pub trait ArgTuple: std::marker::Tuple {
    type Args<Vars: VarTuple>;
    fn args<Vars: VarTuple>() -> Self::Args<Vars>;
}

pub trait VarTuple {
    type Vars<Sig: ArgTuple>;
    fn vars<Sig: ArgTuple>() -> Self::Vars<Sig>;
    fn init() -> Self;
}

impl ArgTuple for () {
    type Args<Vars: VarTuple> = ();

    #[inline(always)]
    fn args<Vars: VarTuple>() -> Self::Args<Vars> {
        ()
    }
}

impl VarTuple for () {
    type Vars<Sig: ArgTuple> = ();

    #[inline(always)]
    fn vars<Sig: ArgTuple>() -> Self::Vars<Sig> {
        ()
    }
    #[inline(always)]
    fn init() -> Self {
        ()
    }
}

pub trait Project<From> {
    type Out: CraneliftValue;
    fn project_ref(t: &From) -> &Self::Out;
    fn project_mut_ref(t: &mut From) -> &mut Self::Out;
    const POS: usize;
}

macro_rules! impl_lensstruct {
    (($structname:ident $typename:ident $num:tt)) => {
        #[derive(Clone, Copy)]
        pub struct $structname;
    };
}

macro_rules! impl_tuple {
    ($(($structname:ident $typename:ident $num:tt))*) => {
        macro_rules! tuple {
            () => (($($typename),*,));
        }
        macro_rules! impl_tup {
            ($inner_structname:ident $inner_typename:ident $inner_num:tt ) => {
                impl <$($typename: CraneliftValue),*,> Project<tuple!()> for $inner_structname {
                    type Out = $inner_typename;
                    #[inline(always)]
                    fn project_ref(t: &tuple!()) -> &$inner_typename {
                        &t.$inner_num
                    }
                    #[inline(always)]
                    fn project_mut_ref(t: &mut tuple!()) -> &mut $inner_typename {
                        &mut t.$inner_num
                    }
                    const POS: usize = $inner_num;
                }
            };
        }
        impl<$($typename: CraneliftValue + VariableValue),*> VarTuple for tuple!() {
            type Vars<Sig: ArgTuple> = ($(Wrap<Sig, Self, Var<Self, $typename, $structname>>),*,);
            #[inline(always)]
            fn init() -> Self {
                ($($typename::default()),*,)
            }
            #[inline(always)]
            fn vars<Sig: ArgTuple>() -> Self::Vars<Sig> {
                ($(
                    Wrap::new(Var{
                        _l: $structname,
                        _sig: PhantomData,
                    })
                ),*,)
            }
        }
        impl<$($typename: CraneliftValue),*> ArgTuple for tuple!() {
            type Args<Vars: VarTuple> = ($(Wrap<Self, Vars, Arg<Self, $typename, $structname>>),*,);
            #[inline(always)]
            fn args<Vars: VarTuple>() -> Self::Args<Vars> {
                ($(
                    Wrap::new(Arg{
                        _l: $structname,
                        _sig: PhantomData,
                    })
                ),*,)
            }
        }
        impl<$($typename: CraneliftValue + VariableValue),*> CraneliftVars for tuple!() {
            fn as_types() -> Vec<ExprType> {
                let mut ret = vec![];
                $(
                let ty = $typename::ty();
                ret.push(ty);
                )*
                ret
            }
        }
        impl<$($typename: CraneliftValue),*> CraneliftArgs for tuple!() {
            type Function<Out> = fn($($typename),*) -> Out;
            fn as_cranelift_arguments() -> Vec<Option<(AbiParam, ExprType)>> {
                let mut ret = vec![];
                $(
                let real_ty = $typename::ty();
                ret.push($typename::as_cranelift_argument().map(|x| (x, real_ty)));
                )*
                ret
            }
        }
        $(
            impl_tup!($structname $typename $num);
        )*
    };
}

macro_rules! scan_and_map {
    (@ $scanner:tt $mapper:tt $($current_args:tt),* | $first:tt, $($other:tt),+) => {
        $scanner!($($current_args)*);
        $mapper!($first);
        scan_and_map!(@ $scanner $mapper $($current_args),*, $first | $($other),+);
    };
    (@ $scanner:tt $mapper:tt $($current_args:tt),* | $first:tt) => {
        $scanner!($($current_args)*);
        $mapper!($first);
        $scanner!($($current_args)* $first);
    };
    ($scanner:tt! $mapper:tt! $first:tt, $($arg:tt),*) => {
        $mapper!($first);
        scan_and_map!(@ $scanner $mapper $first | $($arg),*);
    };
}

scan_and_map!(impl_tuple! impl_lensstruct! (Project0 A 0), (Project1 B 1), (Project2 C 2), (Project3 D 3), (Project4 E 4), (Project5 F 5), (Project6 G 6), (Project7 H 7), (Project8 I 8), (Project9 J 9), (Project10 K 10), (Project11 L 11), (Project12 M 12), (Project13 N 13), (Project14 O 14), (Project15 P 15));
//scan_and_map!(impl_tuple! impl_lensstruct! (Project0 A 0), (Project1 B 1));
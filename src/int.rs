use crate::expr::{ExprType, ExprValue};
use std::ops::*;

pub trait Int:
    Add<Output = Self>
    + Sub<Output = Self>
    + Mul<Output = Self>
    + Div<Output = Self>
    + BitAnd<Output = Self>
    + BitOr<Output = Self>
    + BitXor<Output = Self>
    + Shl<Output = Self>
    + Shr<Output = Self>
    + Rem<Output = Self>
    + Ord
    + Eq
    + Copy
    + ExprValue
{
    const BITS: u16;
    const SIGNED: bool;
}

impl ExprValue for i8 {
    fn ty() -> ExprType {
        ExprType::I8
    }
}
impl Int for i8 {
    const BITS: u16 = 8;

    const SIGNED: bool = true;
}
impl ExprValue for u8 {
    fn ty() -> ExprType {
        ExprType::U8
    }
}
impl Int for u8 {
    const BITS: u16 = 8;

    const SIGNED: bool = false;
}
impl ExprValue for i16 {
    fn ty() -> ExprType {
        ExprType::I16
    }
}
impl Int for i16 {
    const BITS: u16 = 16;

    const SIGNED: bool = true;
}
impl ExprValue for u16 {
    fn ty() -> ExprType {
        ExprType::U16
    }
}
impl Int for u16 {
    const BITS: u16 = 16;

    const SIGNED: bool = false;
}
impl ExprValue for i32 {
    fn ty() -> ExprType {
        ExprType::I32
    }
}
impl Int for i32 {
    const BITS: u16 = 32;

    const SIGNED: bool = true;
}
impl ExprValue for u32 {
    fn ty() -> ExprType {
        ExprType::U32
    }
}
impl Int for u32 {
    const BITS: u16 = 32;

    const SIGNED: bool = false;
}
impl ExprValue for i64 {
    fn ty() -> ExprType {
        ExprType::I64
    }
}
impl Int for i64 {
    const BITS: u16 = 64;

    const SIGNED: bool = true;
}
impl ExprValue for u64 {
    fn ty() -> ExprType {
        ExprType::U32
    }
}
impl Int for u64 {
    const BITS: u16 = 64;

    const SIGNED: bool = false;
}
impl ExprValue for u128 {
    fn ty() -> ExprType {
        ExprType::U128
    }
}
impl Int for usize {
    const BITS: u16 = usize::BITS as u16;

    const SIGNED: bool = false;
}
impl ExprValue for usize {
    fn ty() -> ExprType {
        ExprType::ISize
    }
}
impl Int for isize {
    const BITS: u16 = isize::BITS as u16;

    const SIGNED: bool = true;
}
impl ExprValue for isize {
    fn ty() -> ExprType {
        ExprType::ISize
    }
}
impl Int for i128 {
    const BITS: u16 = 128;

    const SIGNED: bool = true;
}
impl ExprValue for i128 {
    fn ty() -> ExprType {
        ExprType::I128
    }
}
impl Int for u128 {
    const BITS: u16 = 128;

    const SIGNED: bool = false;
}

pub trait UsualArithmeticConverted<Other: Int>: Int {
    type Result: Int;
    fn usual_convert(self, other: Other) -> (Self::Result, Self::Result);
}

impl<I: Int> UsualArithmeticConverted<Self> for I {
    type Result = Self;
    #[inline(always)]
    fn usual_convert(self, other: Self) -> (Self, Self) {
        (self, other)
    }
}
//
//macro_rules! impl_conversion {
//    ($name:ident > $($other:ident)*) => {
//        $(
//        impl UsualArithmeticConverted<$other> for $name {
//            type Result = $name;
//            fn usual_convert(self, other: $other) -> (Self::Result, Self::Result) {
//                (self, other as $name)
//            }
//        }
//        impl UsualArithmeticConverted<$name> for $other {
//            type Result = $name;
//            fn usual_convert(self, other: $name) -> (Self::Result, Self::Result) {
//                (self as $name, other)
//            }
//        }
//        )*
//    };
//    ($name:ident < $($other:ident)*) => {
//        $(
//        impl UsualArithmeticConverted<$other> for $name {
//            type Result = $other;
//            fn usual_convert(self, other: $other) -> (Self::Result, Self::Result) {
//                (self as $other, other)
//            }
//        }
//        impl UsualArithmeticConverted<$name> for $other {
//            type Result = $other;
//            fn usual_convert(self, other: $name) -> (Self::Result, Self::Result) {
//                (self, other as $other)
//            }
//        }
//        )*
//    }
//}
//
//macro_rules! conversion_hierarchy {
//    ($name:ident $($lesser:ident)*) => (
//        impl_conversion!($name > $($lesser)*);
//        conversion_hierarchy!($($lesser)*);
//    );
//    () => ();
//}

//conversion_hierarchy!(u128 i128 u64 i64 usize isize u32 i32 u16 i16 u8 i8);

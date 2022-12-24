#![feature(unboxed_closures, fn_traits, tuple_trait)]
pub mod expr;
mod int;
mod jit;
mod tuples;

use expr::prelude::*;

use crate::jit::CraneliftModule;

fn main() {
    let (index, prime) = <(u32, bool)>::vars();

    let mut ctx = CraneliftModule::default();

    let is_prime = 
    <(u32,)>::func(|number|
        when(number.lt(2)).doth(
            false
        ).otherwise(
            index.set(2)
            .then(prime.set(true))
            .then(
                whilst(prime.and((index * index).le(number))).doth(
                    when((number % index).eq(0)).doth(
                        prime.set(false)
                    )
                    .then(index.set(index + 1)),
                )
            )
            .then(prime),
        )
        .with_specialization()
        .arg(number, 5)
        .build(&mut ctx)
    );
    assert_eq!(is_prime(0), false);
    assert_eq!(is_prime(1), false);
    assert_eq!(is_prime(2), true);
    assert_eq!(is_prime(4), false);
    assert_eq!(is_prime(5), true);
    assert_eq!(is_prime(12435431), true);
}

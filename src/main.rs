use std::rc::Rc;

mod range;

use range::{Range, IntoRange};

fn main() {
    for m in (0..5).into_rng() {
        for n in (0..6).into_rng() {
            for k in (0..7).into_rng() {
                println!("{:?}", m);
                println!("{:?}", k);
                println!("{:?}", n);
            }

        }
    }

}

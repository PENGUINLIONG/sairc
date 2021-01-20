mod range;
mod program;

use range::{rng_to_idxer, Range, IntoRange};

fn main() {
    let m = (0..7).into_rng_named("m");
    let n = (0..9).into_rng_named("n");
    let k = (0..10).into_rng_named("k");

    println!("m={:?}", m);
    println!("k={:?}", k);
    println!("n={:?}", n);

    let (m_mod4, m_div4) = m.split(4);

    println!("m_mod4={:?}", m_mod4);
    println!("m_div4={:?}", m_div4);

    let merged = [m_div4, n, k, m_mod4].iter()
        .collect::<Range>();

    println!("{:?}", rng_to_idxer(&merged));
}

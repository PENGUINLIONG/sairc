use std::rc::Rc;
use std::ops::{Bound, Deref, DerefMut, RangeBounds};
use std::convert::TryInto;



#[derive(Debug)]
struct RangeInner {
    beg: usize,
    end: usize,
    step: usize,
}

#[derive(Debug, Clone)]
pub struct Range(Rc<RangeInner>);

impl Range {
    pub fn new(beg: usize, end: usize, step: usize) -> Self {
        let inner = RangeInner { beg, end, step };
        Range(Rc::new(inner))
    }
}



pub struct IntoIter(Rc<RangeInner>, bool);
impl IntoIter {
    fn new(inner: Rc<RangeInner>) -> IntoIter {
        IntoIter(inner, false)
    }
}

impl IntoIterator for Range {
    type IntoIter = IntoIter;
    type Item = Range;
    fn into_iter(self) -> Self::IntoIter {
        IntoIter::new(self.0.clone())
    }
}
impl Iterator for IntoIter {
    type Item = Range;

    fn next(&mut self) -> Option<Self::Item> {
        if self.1 {
            None
        } else {
            self.1 = true;
            Some(Range(self.0.clone()))
        }
    }
}



pub trait IntoRange<T: TryInto<usize> + Clone> : RangeBounds<T> + Sized {
    fn into_rng(self) -> Range {
        let beg = match self.start_bound() {
            Bound::Included(x) | Bound::Excluded(x) => {
                if let Ok(x) = x.clone().try_into() {
                    x
                } else {
                    panic!("range start bound cannot be represented as `usize`");
                }
            },
            Bound::Unbounded => 0,
        };
        let end = match self.end_bound() {
            Bound::Included(x) | Bound::Excluded(x) => {
                if let Ok(x) = x.clone().try_into() {
                    x
                } else {
                    panic!("range start bound cannot be represented as `usize`");
                }
            },
            Bound::Unbounded => panic!("range cannot be unbound"),
        };

        Range::new(beg, end, 1)
    }
}
impl<T: TryInto<usize> + Clone> IntoRange<T> for std::ops::Range<T> {}

use std::rc::Rc;
use std::ops::{Bound, RangeBounds};
use std::borrow::Borrow;
use std::convert::TryInto;



#[derive(Clone, Debug)]
struct RootRange {
    name: Option<String>,
    beg: usize,
    end: usize,
    pitch: usize,
}
#[derive(Clone, Debug)]
struct SplitRange {
    parent: Rc<RangeInner>,
    stride: usize,
}
#[derive(Clone, Debug)]
struct MergeRange {
    near: Rc<RangeInner>,
    far: Rc<RangeInner>,
}
#[derive(Debug)]
enum RangeInner {
    Root(RootRange),
    SplitMod(SplitRange),
    SplitDiv(SplitRange),
    Merge(MergeRange),
}
fn get_rng_len(inner: &RangeInner) -> usize {
    match inner {
        RangeInner::Root(root) => root.pitch,
        RangeInner::SplitMod(split) => split.stride,
        RangeInner::SplitDiv(split) => {
            (get_rng_len(&split.parent) + split.stride - 1) / split.stride
        },
        RangeInner::Merge(merge) => {
            get_rng_len(&merge.far) * get_rng_len(&merge.near)
        },
    }
}

#[derive(Clone, Debug)]
pub struct Range(Rc<RangeInner>);

impl Range {
    pub fn new(beg: usize, end: usize) -> Self {
        Self::new_aligned(beg, end, 1)
    }
    pub fn new_aligned(beg: usize, end: usize, align: usize) -> Self {
        let pitch = (end + align - 1) / align;
        let root = RootRange { name: None, beg, end, pitch };
        let inner = RangeInner::Root(root);
        Range(Rc::new(inner))
    }
    pub fn with_name(name: &str, beg: usize, end: usize) -> Self {
        Self::with_name_aligned(name, beg, end, 1)
    }
    pub fn with_name_aligned(name: &str, beg: usize, end: usize, align: usize) -> Self {
        let pitch = (end + align - 1) / align;
        let root = RootRange { name: Some(name.to_owned()), beg, end, pitch };
        let inner = RangeInner::Root(root);
        Range(Rc::new(inner))
    }

    pub fn len(&self) -> usize {
        get_rng_len(&*self.0)
    }

    pub fn split(&self, stride: usize) -> (Self, Self) {
        let split = SplitRange {
            parent: self.0.clone(),
            stride,
        };
        let inner_mod = RangeInner::SplitMod(split.clone());
        let inner_div = RangeInner::SplitDiv(split);
        (Range(Rc::new(inner_mod)), Range(Rc::new(inner_div)))
    }
    pub fn merge(&self, far: &Self) -> Self {
        let merge = MergeRange {
            near: self.0.clone(),
            far: far.0.clone(),
        };
        let inner = RangeInner::Merge(merge);
        Range(Rc::new(inner))
    }
}
impl<Rng: std::borrow::Borrow<Range>> std::iter::FromIterator<Rng> for Range {
    fn from_iter<T: IntoIterator<Item=Rng>>(iter: T) -> Range {
        let rngs = iter.into_iter().collect::<Vec<_>>();
        assert!(rngs.len() != 0, "cannot collect a range from an emtpy slice");

        let mut rv: Range = rngs
            .last()
            .unwrap()
            .borrow()
            .clone();
        for rng in rngs.iter().rev().skip(1) {
            let rng: &Range = rng.borrow();
            rv = rv.merge(rng);
        }
        rv
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

fn get_rng_bound<T: TryInto<usize> + Clone, Rng: RangeBounds<T>>(rng: &Rng) -> (usize, usize) {
    let beg = match rng.start_bound() {
        Bound::Included(x) | Bound::Excluded(x) => {
            if let Ok(x) = x.clone().try_into() {
                x
            } else {
                panic!("range start bound cannot be represented as `usize`");
            }
        },
        Bound::Unbounded => 0,
    };
    let end = match rng.end_bound() {
        Bound::Included(x) | Bound::Excluded(x) => {
            if let Ok(x) = x.clone().try_into() {
                x
            } else {
                panic!("range start bound cannot be represented as `usize`");
            }
        },
        Bound::Unbounded => panic!("range cannot be unbound"),
    };
    (beg, end)
}

pub trait IntoRange<T: TryInto<usize> + Clone> : RangeBounds<T> + Sized {
    fn into_rng(self) -> Range {
        let (beg, end) = get_rng_bound(&self, );
        Range::new(beg, end)
    }
    fn into_rng_named(self, name: &str) -> Range {
        let (beg, end) = get_rng_bound(&self);
        Range::with_name(name, beg, end)
    }
}
impl<T: TryInto<usize> + Clone> IntoRange<T> for std::ops::Range<T> {}



// Returns range literal and block stride.
fn rng_to_idxer_impl(inner: &RangeInner) -> String {
    match inner {
        RangeInner::Root(root) => {
            root.name.as_ref().unwrap().to_owned()
        },
        RangeInner::SplitDiv(split) => {
            format!("({} / {})", rng_to_idxer_impl(&split.parent), split.stride)
        },
        RangeInner::SplitMod(split) => {
            format!("({} % {})", rng_to_idxer_impl(&split.parent), split.stride)
        },
        RangeInner::Merge(merge) => {
            format!("({} * {} + {})",
                rng_to_idxer_impl(&merge.far),
                get_rng_len(&merge.near),
                rng_to_idxer_impl(&merge.near))
        }
    }
}
pub fn rng_to_idxer(rng: &Range) -> String {
    rng_to_idxer_impl(&*rng.0)
}

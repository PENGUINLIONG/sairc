use std::borrow::Borrow;
use std::iter::FromIterator;
use std::rc::Rc;

#[derive(Clone, Debug, Default)]
struct FixedRange {
    // Offset in the minimal addressible unit.
    offset: usize,
    // Stride between two steps.
    stride: usize,
    // Total number of steps.
    size: usize,
}
impl From<usize> for FixedRange {
    fn from(x: usize) -> Self {
        FixedRange {
            offset: 0,
            stride: 1,
            size: x,
        }
    }
}

// Small-stride dimension first.
#[derive(Clone, Debug, Default)]
struct Shape(Vec<Option<FixedRange>>);
impl Shape {
    fn size(&self) -> Option<usize> {
        self.0.iter()
            .fold(Some(1), |acc, rng| {
                if let (Some(a), Some(b)) = (acc, rng) {
                    Some(a * b.size)
                } else {
                    None
                }
            })
    }
    fn is_fixed(&self) -> bool {
        self.size().is_some()
    }
}
impl FromIterator<Option<usize>> for Shape {
    fn from_iter<I: IntoIterator<Item=Option<usize>>>(iter: I) -> Self {
        let inner = iter.into_iter()
            .map(|x| x.map(|x| x.into()))
            .collect();
        Shape(inner)
    }
}
impl FromIterator<usize> for Shape {
    fn from_iter<I: IntoIterator<Item=usize>>(iter: I) -> Self {
        iter.into_iter().map(|x| Some(x)).collect()
    }
}
impl FromIterator<Ast> for Shape {
    fn from_iter<I: IntoIterator<Item=Ast>>(iter: I) -> Self {
        iter.into_iter()
            .map(|ast| {
                match *ast.0 {
                    AstInner::Constant(Constant::Int(x)) => Some(x as usize),
                    AstInner::Symbol(sym) if sym.shape.is_empty() => None,
                    _ => panic!("shape descriptor must be integer or a scalar \
                        symbol"),
                }
            })
            .collect()
    }
}






#[derive(Clone, Debug)]
enum Constant {
    Int(i64),
    Float(f64),
    Operation(String),
}
#[derive(Clone, Debug)]
struct Symbol {
    name: String,
    shape: Vec<Ast>,
}
// All constituent content components MUST have the same size.
#[derive(Clone, Debug)]
struct Data {
    content: Vec<Ast>,
}


#[derive(Clone, Debug)]
struct Load {
    name: String,
    shape: Vec<Ast>,
}
#[derive(Clone, Debug)]
struct Store {
    name: String,
    data: Ast,
}
#[derive(Clone, Debug)]
struct Range {
    offset: Ast,
    size: Ast,
    step: Ast,
}
#[derive(Clone, Debug)]
struct Map {
    op: Ast,
    datas: Vec<Ast>,
}
#[derive(Clone, Debug)]
struct Reduce {
    op: Ast,
    data: Ast,
    dim: usize,
}
#[derive(Clone, Debug)]
struct Broadcast {
    data: Ast,
    dim: usize,
    rng: Ast,
}
#[derive(Clone, Debug)]
struct Split {
    data: Ast,
    dim: usize,
    stride: Ast,
}
#[derive(Clone, Debug)]
struct Transpose {
    data: Ast,
    dims: Vec<usize>,
}
#[derive(Clone, Debug)]
struct Merge {
    data: Ast,
    dim_dims: Vec<Vec<usize>>,
}

#[derive(Clone, Debug)]
enum AstInner {
    Constant(Constant),
    Symbol(Symbol),
    Data(Data),

    Load(Load),
    Store(Store),
    Range(Range),
    Map(Map),
    Reduce(Reduce),
    Broadcast(Broadcast),
    Split(Split),
    Transpose(Transpose),
    Merge(Merge),
}




macro_rules! impl_ty_checks {
    ($name:ident, $ty:ident) => {
        fn $name(&self) -> bool {
            if let AstInner::$ty(_) = *self.0 { true } else { false }
        }
    };
}





#[derive(Clone, Debug)]
struct Ast(Rc<AstInner>);
impl Ast {
    fn wrap(inner: AstInner) -> Self {
        Ast(Rc::new(inner))
    }

    // Constant constructors.
    fn int(x: i64) -> Self {
        Self::wrap(AstInner::Constant(Constant::Int(x)))
    }
    fn float(x: f64) -> Self {
        Self::wrap(AstInner::Constant(Constant::Float(x)))
    }
    fn op(name: &str) -> Self {
        Ast::wrap(AstInner::Constant(Constant::Operation(name.to_owned())))
    }

    // Symbol constructor.
    fn sym(name: &str, shape: Vec<Ast>) -> Self {
        let sym = Symbol {
            name: name.to_owned(),
            shape,
        };
        Ast::wrap(AstInner::Symbol(sym))
    }

    // Behavioral symbols.
    fn load(name: &str, shape: Vec<Ast>) -> Self {
        Ast::wrap(AstInner::Load(Load { name: name.to_owned(), shape }))
    }
    fn store(name: &str, data: Ast) -> Self {
        Ast::wrap(AstInner::Store(Store { name: name.to_owned(), data }))
    }
    fn rng(offset: Ast, size: Ast, step: Ast) -> Self {
        Ast::wrap(AstInner::Range(Range { offset, size, step }))
    }
    fn map(op: Ast, datas: Vec<Ast>) -> Self {
        Ast::wrap(AstInner::Map(Map { op, datas }))
    }
    fn reduce(op: Ast, data: Ast, dim: usize) -> Self {
        Ast::wrap(AstInner::Reduce(Reduce { op, data, dim }))
    }
    fn broadcast(data: Ast, dim: usize, rng: Ast) -> Self {
        Ast::wrap(AstInner::Broadcast(Broadcast { data, dim, rng }))
    }
    fn split(data: Ast, dim: usize, stride: Ast) -> Self {
        Ast::wrap(AstInner::Split(Split { data, dim, stride }))
    }
    fn transpose(data: Ast, dims: Vec<usize>) -> Self {
        Ast::wrap(AstInner::Transpose(Transpose { data, dims }))
    }
    fn merge(data: Ast, dim_dims: Vec<Vec<usize>>) -> Self {
        Ast::wrap(AstInner::Merge(Merge { data, dim_dims }))
    }

    impl_ty_checks!(is_const, Constant);
    impl_ty_checks!(is_sym, Symbol);
    impl_ty_checks!(is_data, Data);
    impl_ty_checks!(is_load, Load);
    impl_ty_checks!(is_store, Store);
    impl_ty_checks!(is_rng, Range);
    impl_ty_checks!(is_map, Map);
    impl_ty_checks!(is_reduce, Reduce);
    impl_ty_checks!(is_broadcast, Broadcast);
    impl_ty_checks!(is_split, Split);
    impl_ty_checks!(is_transpose, Transpose);
    impl_ty_checks!(is_merge, Merge);

    fn is_int_const(&self) -> bool {
        if let AstInner::Constant(Constant::Int(_)) = *self.0 {
            true
        } else { false }
    }
    fn is_float_const(&self) -> bool {
        if let AstInner::Constant(Constant::Float(_)) = *self.0 {
            true
        } else { false }
    }



    fn shape(&self) -> Shape {
        match *self.0 {
            AstInner::Constant(_) => Shape::default(),
            AstInner::Symbol(sym) => sym.shape.iter().cloned().collect(),
            AstInner::Data(data) => {
                let Some(first) = data.content.first();
                let mut shape = first.shape();
                shape.0.push(Some(data.content.len()));
                shape
            },
            AstInner::Load(load) => load.shape.iter().cloned().collect(),
            AstInner::Store(store) => store.data.shape(),
            AstInner::Range(rng) => rng.data.shape()
        }
    }

    fn size(&self) -> Option<usize> {
        

        match *self.0 {
            AstInner::Constant(_) => Some(1),
            AstInner::Symbol(sym) => {
                let mut res = 1;
                for ast in sym.shape {
                    if let AstInner::Constant(Constant::Int(x)) = *ast.0 {
                        res *= x as usize;
                    } else {
                        return None;
                    }
                }
                Some(res)
            },
            AstInner::Data(data) => {
                let mut res = 0;
                data.content.iter()
                    .filter_map(|x| x.size())
                    .next()
                    .map(|x| x * data.content.len())
            },
            AstInner::Load(load) => {
                load.
            }
        }
    }
}

fn main() {
    let zero = Ast::int(0);
    let one = Ast::int(1);

    let m = Ast::sym("m", vec![]);
    let k = Ast::sym("k", vec![]);
    let n = Ast::sym("n", vec![]);

    let m_rng = Ast::rng(zero.clone(), m.clone(), one.clone());
    let n_rng = Ast::rng(zero.clone(), n.clone(), one.clone());

    let a = Ast::load("A", vec![m.clone(), k.clone()]);
    let b = Ast::load("B", vec![k.clone(), n.clone()]);
    
    let result = Ast::reduce(Ast::op("+"), Ast::map(Ast::op("*"), vec![
        Ast::broadcast(a, 2, n_rng),
        Ast::broadcast(b, 0, m_rng)]), 1);
        
    let c = Ast::store("C", result);

    print!("{:?}", c);


}

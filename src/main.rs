use std::str::FromStr;
use std::iter::FromIterator;
use std::collections::HashMap;

#[derive(Debug)]
enum Token<'a> {
    Open,
    Close,
    Item(&'a str),
}

struct Tokenize<'a> {
    src: &'a str,
}
impl<'a> Tokenize<'a> {
    pub fn new(src: &'a str) -> Tokenize<'a> {
        Tokenize { src }
    }
}
impl<'a> Iterator for Tokenize<'a> {
    type Item = Token<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        let mut ichars = self.src.char_indices().peekable();
        let mut in_comment = false;
        let mut in_item = false;

        let mut token = None;
        let mut beg = 0;
        let mut end = 0;

        while let Some((ichar, c)) = ichars.next() {
            let ichar_next = ichars.peek()
                .map(|x| x.0)
                .unwrap_or(self.src.len());

            if c == '"' {
                // Ignore comments in quotes.
                in_comment = !in_comment;
            } else if !in_comment {
                if in_item {
                    // The parser is currently trying to extract an item. Try to
                    // find an token boundary.
                    match c {
                        '(' | ')' | ' ' | '\r' | '\n' | '\t' => {
                            token = Some(Token::Item(&self.src[beg..end]));
                        },
                        _ => end = ichar_next,
                    }
                } else {
                    // The parser is not extracting an item so we can extract
                    // other language constructs.
                    match c {
                        '(' => token = Some(Token::Open),
                        ')' => token = Some(Token::Close),
                        ' ' | '\r' | '\n' | '\t' => {},
                        _ => {
                            // Start item parsing.
                            in_item = true;
                            beg = ichar;
                        }
                    }
                    end = ichar_next;
                }
            }

            if token.is_some() { break }
        }
        self.src = &self.src[end..];
        token
    }
}

fn tokenize<'a>(src: &'a str) -> impl Iterator<Item=Token<'a>> { Tokenize::new(src) }


#[derive(Clone, Copy)]
enum Constant {
    Integer(isize),
    Float(f64),
    Boolean(bool),
}
impl std::fmt::Debug for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::Integer(x) => write!(f, "{}", x),
            Constant::Float(x) => write!(f, "{}", x),
            Constant::Boolean(x) => write!(f, "{}", x),
        }
    }
}

#[derive(Clone, Copy)]
enum ContextValue<'a> {
    Name(&'a str),
    Constant(Constant),
    Reference(ExpressionId),
}
impl<'a> ContextValue<'a> {
    pub fn parse(lit: &'a str) -> ContextValue<'a> {
        if let Ok(x) = FromStr::from_str(lit) {
            return ContextValue::Constant(Constant::Boolean(x));
        }
        if let Ok(x) = FromStr::from_str(lit) {
            return ContextValue::Constant(Constant::Integer(x));
        }
        if let Ok(x) = FromStr::from_str(lit) {
            return ContextValue::Constant(Constant::Float(x));
        }
        ContextValue::Name(lit)
    }
}
impl<'a> std::fmt::Debug for ContextValue<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ContextValue::Name(name) => write!(f, "@{}", name),
            ContextValue::Constant(x) => write!(f, "{:?}", x),
            ContextValue::Reference(iexpr) => write!(f, "#{}", iexpr),
        }
    }
}

struct Expression<'a> {
    cvals: Vec<ContextValue<'a>>,
}
impl<'a> Expression<'a> {
    pub fn op(&self) -> &'a str {
        if let Some(ContextValue::Name(x)) = self.cvals.first() {
            x
        } else {
            panic!("op name missing");
        }
    }
    pub fn args(&self) -> &[ContextValue<'a>] {
        &self.cvals[1..]
    }
    pub fn get_arg(&self, i: usize) -> ContextValue<'a> {
        self.cvals[i + 1]
    }
    pub fn get_name(&self, i: usize) -> Option<&'a str> {
        if let ContextValue::Name(x) = self.get_arg(i) {
            Some(x)
        } else { None }
    }
}
impl<'a> std::fmt::Debug for Expression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {:?}", self.op(), self.args())
    }
}

struct Flow<'a> {
    exprs: Vec<Expression<'a>>,
}
impl<'a> Flow<'a> {
    pub fn parse<I: IntoIterator<Item=Token<'a>>>(tokens: I) -> Flow<'a> {
        let mut stack = vec![vec![ContextValue::Name("module")]];
        let mut exprs = Vec::new();

        for token in tokens {
            match token {
                Token::Open => stack.push(Vec::new()),
                Token::Close => {
                    let cvals = stack.pop().unwrap();
                    assert_ne!(cvals.len(), 0, "expression must have an op");
                    
                    let iexpr = exprs.len();
                    let expr = Expression { cvals };

                    exprs.push(expr);
                    stack.last_mut()
                        .unwrap()
                        .push(ContextValue::Reference(iexpr));
                },
                Token::Item(lit) => {
                    stack.last_mut()
                        .unwrap()
                        .push(ContextValue::parse(lit))
                },
            }
        }

        // Don't forget the global scope.
        let global_cvals = stack.pop().unwrap();
        let global_expr = Expression { cvals: global_cvals };
        exprs.push(global_expr);

        Flow { exprs }
    }
}
impl<'a> IntoIterator for Flow<'a> {
    type IntoIter = std::vec::IntoIter<Expression<'a>>;
    type Item = Expression<'a>;
    fn into_iter(self) -> Self::IntoIter {
        self.exprs.into_iter()
    }
}
impl<'a> std::fmt::Debug for Flow<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Flow ")?;
        let mut x = f.debug_map();
        for (i, expr) in self.exprs.iter().enumerate() {
            x.entry(&i, expr);
        }
        x.finish()
    }
}



static SRC: &'static str = r#"
"Define types."
(type u32 (uint 32))
(type f32 (float 32))



"Declare variables."
(var M u32)
(var K u32)
(var N u32)
(var A (tensor f32 M K))
(var B (tensor f32 K N))
(var C (tensor f32 M N))


"Declare loop control variables."
(var m u32)
(var k u32)
(var n u32)
(var temp f32)

"Inject variables from arguments."
(let M (arg 0 u32))
(let K (arg 1 u32))
(let N (arg 2 u32))
(let A (arg 3 (tensor f32 M K)))
(let B (arg 4 (tensor f32 K N)))
(let C (arg 5 (tensor f32 M N)))

"Loop and MatMul."
(let m (range 0 M 1))
(let n (range 0 N 1))
(let k (range 0 K 1))
(let temp 0.0)
(let temp (recur temp (+ (* (load A m k) (load B k n)) temp)))
(store temp C m n)
"#;




type ExpressionId = usize;
const INVALID_EXPRESSION: ExpressionId = std::usize::MAX;

type ValueId = usize;


#[derive(Debug)]
struct Variable<'a> {
    name: &'a str,
    created_at: ExpressionId,
    /// The binding in time sequence, early bindings are pushed first. Becuase
    /// the expressions comes in order we can use this sequence information to
    /// determine the variable is bound at which time.
    binds: Vec<(ExpressionId, ValueId)>,
}



#[derive(Debug)]
struct ExpressionMeta<'a> {
    expr: Expression<'a>,
    children: Vec<ExpressionId>,
    parent: ExpressionId,
}

#[derive(Debug, Default)]
struct ModuleManifest<'a> {
    /// All expressions in this module and their metadata.
    exprs: Vec<ExpressionMeta<'a>>,
}
impl<'a> ModuleManifest<'a> {
    fn get_expr_children(&self, iexpr: ExpressionId) -> &[ExpressionId] {
        &self.exprs[iexpr].children
    }
    fn record_expr(&mut self, expr: Expression<'a>) -> ExpressionId {
        // Collect common expression metadata first.
        let children = expr.args()
            .iter()
            .filter_map(|x| {
                if let ContextValue::Reference(child) = x {
                    Some(child)
                } else { None }
            })
            .flat_map(|&direct_child| {
                self.get_expr_children(direct_child)
                    .iter()
                    .copied()
                    .chain(std::iter::once(direct_child))
            })
            .collect::<Vec<_>>();
        let iexpr = self.exprs.len();

        // Assign parent ID to children.
        for arg in expr.args().iter() {
            if let ContextValue::Reference(ichild) = arg {
                self.exprs.get_mut(*ichild).unwrap().parent = iexpr;
            }
        }

        // Record expression.
        let meta = ExpressionMeta {
            expr,
            children,
            parent: INVALID_EXPRESSION,
        };
        self.exprs.push(meta);
        iexpr
    }
}




#[derive(Debug, Default)]
struct NamespaceStackFrame<'a> {
    bind_map: HashMap<&'a str, ContextValue<'a>>,
}
#[derive(Debug, Default)]
struct NamespaceStack<'a> {
    ns_stack: Vec<NamespaceStackFrame<'a>>,
}
impl<'a> NamespaceStack<'a> {
    fn push(&mut self) {
        self.ns_stack.push(NamespaceStackFrame::default());
    }
    fn pop(&mut self) {
        self.ns_stack.pop();
    }
    fn declare(&mut self, name: &'a str) {
        use std::collections::hash_map::Entry::Vacant;
        let last_frame = self.ns_stack.last_mut().unwrap();
        if let Vacant(e) = last_frame.bind_map.entry(name) {
            e.insert(ContextValue::Reference(INVALID_EXPRESSION));
        }
    }
    fn bind(&mut self, name: &'a str, cval: ContextValue<'a>) {
        use std::collections::hash_map::Entry::Occupied;
        for frame in self.ns_stack.iter_mut().rev() {
            if let Occupied(mut e) = frame.bind_map.entry(name) {
                e.insert(cval);
                return;
            }
        }
        panic!("name not yet declared");
    }
    fn look_up(&self, name: &str) -> ContextValue<'a> {
        for frame in self.ns_stack.iter().rev() {
            if let Some(cval) = frame.bind_map.get(name) {
                return *cval;
            }
        }
        panic!("name not yet bound")
    }
}



// Re-writer functions.
impl<'a> ModuleManifest<'a> {
    fn flatten_aliases_impl(
        &self,
        meta: &ExpressionMeta<'a>,
        ns_stack: &mut NamespaceStack<'a>,
        out_manifest: &mut ModuleManifest<'a>
    ) -> Option<ExpressionId> {
        let mut rv = None;
        match meta.expr.op() {
            "var" => {
                let var_name = meta.expr.get_name(0)
                    .expect("name is missing in `var` op");
                ns_stack.declare(var_name);
            },
            "let" => {
                let var_name = meta.expr.get_name(0)
                    .expect("name is missing in `let` op");

                let cval = match meta.expr.get_arg(1) {
                    ContextValue::Name(name) => {
                        // The returned ID is already in the new context.
                        ns_stack.look_up(name)
                    },
                    ContextValue::Constant(c) => ContextValue::Constant(c),
                    ContextValue::Reference(iexpr) => {
                        // This `iexpr` is in the old context in `self`.
                        // Flatten and transfer it to the new context.
                        let out_iexpr = self.flatten_aliases_impl(&self.exprs[iexpr], ns_stack, out_manifest)
                            .expect("reference failed");
                        ContextValue::Reference(out_iexpr)
                    },
                };

                ns_stack.bind(var_name, cval);
            },
            "type" => {
                let ty_name = meta.expr.get_name(0)
                    .expect("name is missing in `type` op");
                let cval = match meta.expr.get_arg(1) {
                    ContextValue::Name(name) => {
                        ns_stack.look_up(name)
                    },
                    ContextValue::Reference(iexpr) => {
                        let out_iexpr = self.flatten_aliases_impl(&self.exprs[iexpr], ns_stack, out_manifest)
                            .expect("reference failed");
                        ContextValue::Reference(out_iexpr)
                    },
                    _ => panic!("invalid context value in `type` op"),
                };
                ns_stack.declare(ty_name);
                ns_stack.bind(ty_name, cval);
            },
            _ => {
                ns_stack.push();

                let mut out_cvals = vec![ContextValue::Name(meta.expr.op())];
                for arg in meta.expr.args() {
                    let out_cval = match arg {
                        ContextValue::Name(name) => {
                            ns_stack.look_up(name)
                        },
                        ContextValue::Constant(c) => ContextValue::Constant(*c),
                        ContextValue::Reference(iexpr) => {
                            let child_expr = &self.exprs[*iexpr];
                            if let Some(out_iexpr) =
                                self.flatten_aliases_impl(child_expr, ns_stack, out_manifest)
                            {
                                ContextValue::Reference(out_iexpr)
                            } else {
                                continue;
                            }
                        },
                    };

                    out_cvals.push(out_cval);
                }
                let out_expr = Expression { cvals: out_cvals };
                rv = Some(out_manifest.record_expr(out_expr));

                ns_stack.pop();
            }
        }

        return rv;
    }
    pub fn flatten_aliases(&self) -> ModuleManifest<'a> {
        let root_expr = self.exprs.last()
            .expect("empty manifest");

        let mut ns_stack = NamespaceStack::default();
        let mut out_manifest = ModuleManifest::default();
        self.flatten_aliases_impl(&root_expr, &mut ns_stack, &mut out_manifest);
        out_manifest.exprs.reverse();

        out_manifest
    }

    fn to_source_impl(&self, expr: &ExpressionMeta<'a>) -> String {
        let mut out_source = String::new();
        out_source.push_str("(");
        let expr_lit = expr.expr.cvals.iter()
            .map(|cval| {
                match cval {
                    ContextValue::Name(name) => {
                        name.to_string()
                    },
                    ContextValue::Constant(c) => {
                        format!("{:?}", c)
                    },
                    ContextValue::Reference(iexpr) => {
                        let expr = self.exprs.get(*iexpr)
                            .expect("broken reference");
                        self.to_source_impl(expr)
                    }
                }
            })
            .collect::<Vec<_>>();
        out_source.push_str(&expr_lit.join(" "));
        out_source.push_str(")");
        out_source
    }
    /// Translate back to source.
    pub fn to_source(&self) -> String {
        let source = self.to_source_impl(self.exprs.last().unwrap());
        source
    }
}
impl<'a> FromIterator<Expression<'a>> for ModuleManifest<'a> {
    fn from_iter<I: IntoIterator<Item=Expression<'a>>>(exprs: I) -> Self {
        let mut manifest = ModuleManifest::default();
        for expr in exprs {
            manifest.record_expr(expr);
        }
        manifest
    }
}

















fn main() {
    let tokens = tokenize(SRC).collect::<Vec<_>>();
    //println!("{:?}", tokens);
    let flow = Flow::parse(tokens);
    let manifest = flow.into_iter().collect::<ModuleManifest<'_>>();
    let flattened_manifest = manifest.flatten_aliases();
    let flattened_source = flattened_manifest.to_source();
    println!("{}", flattened_source);
}

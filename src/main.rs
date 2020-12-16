use std::str::FromStr;

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

enum ContextValue<'a> {
    Name(&'a str),
    Constant(Constant),
    Reference(usize),
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
    pub fn op(&self) -> Option<&'a str> {
        if let Some(ContextValue::Name(x)) = self.cvals.first() {
            Some(x)
        } else {
            None
        }
    }
    pub fn args(&self) -> &[ContextValue<'a>] {
        &self.cvals[1..]
    }
}
impl<'a> std::fmt::Debug for Expression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {:?}", self.op().unwrap(), self.args())
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
impl<'a> std::fmt::Debug for Flow<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Flow ");
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



(entry matmul

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
(let M (arg 0))
(let K (arg 1))
(let N (arg 2))
(let A (arg 3))
(let B (arg 4))
(let C (arg 5))

"Loop and MatMul."
(loop m (range 0 M)
    (loop n (range 0 N)
        (let temp 0.0)
        (loop k (range 0 K)
        (let temp (+ (* (load A m k) (load B k n)) temp))
        )
        (store temp C m n)
    )
)

)
"#;


fn main() {
    let tokens = tokenize(SRC).collect::<Vec<_>>();
    //println!("{:?}", tokens);
    let flow = Flow::parse(tokens);
    println!("{:#?}", flow);
}

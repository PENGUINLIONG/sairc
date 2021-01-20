
#[derive(Clone, Debug)]
pub enum Token<'a> {
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

pub fn tokenize<'a>(src: &'a str) -> impl Iterator<Item=Token<'a>> {
    Tokenize::new(src)
}

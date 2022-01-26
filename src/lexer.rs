use tokens::{Equal, Make, Token};

use self::tokens::{Ident, LessThan, Plus};

mod tokens {
    use crate::lexer::first_char;

    use super::{LexError, LexingResult};

    #[derive(Clone, Copy, Debug, PartialEq)]
    pub(crate) enum Token<'input> {
        Ident(Ident<'input>),
        Int(Int),

        Equal(Equal),
        LessThan(LessThan),
        Plus(Plus),

        If(If),
        Else(Else),
        Let(Let),
    }

    pub(crate) trait Make<'input>
    where
        Self: 'input + Into<Token<'input>>,
    {
        fn make(input: &'input str) -> LexingResult<'input> {
            Self::parse(input).map(|(tail, token)| (tail, token.into()))
        }

        fn parse(input: &'input str) -> Result<(&'input str, Self), LexError>;
    }

    #[derive(Clone, Copy, Debug, PartialEq)]
    pub(crate) struct Ident<'input>(&'input str);

    impl<'input> Make<'input> for Ident<'input> {
        fn make(input: &'input str) -> LexingResult<'input> {
            Ident::parse(input).map(|(tail, token)| (tail, token.check_keyword()))
        }

        fn parse(input: &'input str) -> Result<(&'input str, Ident<'input>), LexError> {
            let idx = input
                .char_indices()
                .find(|(_, chr)| !chr.is_alphanumeric() && *chr != '_')
                .map(|(idx, _)| idx)
                .unwrap_or_else(|| input.len());

            debug_assert_ne!(
                idx, 0,
                "Ident::parse is called with a string which does not start with an alphabetic char",
            );

            let (matched, tail) = input.split_at(idx);

            let ident = Ident(matched);

            Ok((tail, ident))
        }
    }

    impl<'input> Ident<'input> {
        fn check_keyword(self) -> Token<'input> {
            match self.0 {
                "if" => Token::If(If),
                "else" => Token::Else(Else),
                "let" => Token::Let(Let),

                _ => self.into(),
            }
        }
    }

    impl<'input> From<Ident<'input>> for Token<'input> {
        fn from(t: Ident<'input>) -> Token<'input> {
            Token::Ident(t)
        }
    }

    #[derive(Clone, Copy, Debug, PartialEq)]
    pub(crate) struct Int(i32);

    impl<'input> Make<'input> for Int {
        fn parse(input: &'input str) -> Result<(&'input str, Self), LexError> {
            let idx = input
                .char_indices()
                .find(|(_, chr)| !chr.is_numeric())
                .map(|(idx, _)| idx)
                .unwrap_or_else(|| input.len());

            debug_assert_ne!(
                idx, 0,
                "Number::parse is called with a string which does not start with a digit char",
            );

            let (matched, tail) = input.split_at(idx);
            let value = matched.parse().unwrap();
            let int = Int(value);

            Ok((tail, int))
        }
    }

    impl<'input> From<Int> for Token<'input> {
        fn from(t: Int) -> Token<'input> {
            Token::Int(t)
        }
    }

    #[derive(Clone, Copy, Debug, PartialEq)]
    pub(crate) struct If;

    #[derive(Clone, Copy, Debug, PartialEq)]
    pub(crate) struct Else;

    #[derive(Clone, Copy, Debug, PartialEq)]
    pub(crate) struct Let;

    #[derive(Clone, Copy, Debug, PartialEq)]
    pub(crate) struct Plus;

    impl<'input> Make<'input> for Plus {
        fn parse(input: &'input str) -> Result<(&'input str, Plus), LexError> {
            let tail = eat_single_char_token(input, '+');
            let plus = Plus;

            Ok((tail, plus))
        }
    }

    impl<'a> From<Plus> for Token<'a> {
        fn from(t: Plus) -> Token<'a> {
            Token::Plus(t)
        }
    }

    #[derive(Clone, Copy, Debug, PartialEq)]
    pub(crate) struct Equal;

    impl<'input> Make<'input> for Equal {
        fn parse(input: &'input str) -> Result<(&'input str, Equal), LexError> {
            let tail = eat_single_char_token(input, '=');
            let eq = Equal;

            Ok((tail, eq))
        }
    }

    impl<'a> From<Equal> for Token<'a> {
        fn from(t: Equal) -> Token<'a> {
            Token::Equal(t)
        }
    }

    #[derive(Clone, Copy, Debug, PartialEq)]
    pub(crate) struct LessThan;

    impl<'input> Make<'input> for LessThan {
        fn parse(input: &'input str) -> Result<(&'input str, LessThan), LexError> {
            let tail = eat_single_char_token(input, '<');
            let lt = LessThan;

            Ok((tail, lt))
        }
    }

    impl<'input> From<LessThan> for Token<'input> {
        fn from(t: LessThan) -> Token<'input> {
            Token::LessThan(t)
        }
    }

    fn eat_single_char_token(input: &str, chr: char) -> &str {
        debug_assert_eq!(
            first_char(input),
            Some(chr),
            "`eat_single_char_token(_, {:?})` called with input not starting with `{:?}`",
            chr,
            chr,
        );

        input.split_at(chr.len_utf8()).1
    }
}

pub(crate) struct Lexer<'input> {
    tail: &'input str,
}

impl<'input> Lexer<'input> {
    pub(crate) fn new(input: &'input str) -> Lexer<'input> {
        Lexer { tail: input }
    }

    pub(crate) fn tokens(&self) -> Result<Vec<Token<'input>>, LexError> {
        let mut input = self.tail;
        let mut toks = Vec::new();

        while let Some(rslt) = lex_single(input) {
            let (tail, tok) = rslt?;
            toks.push(tok);
            input = tail;
        }

        Ok(toks)
    }
}

fn lex_single<'input>(input: &'input str) -> Option<LexingResult<'input>> {
    let input = whitespaces_opt(input);
    let first_char = first_char(input)?;

    Some(match first_char {
        '=' => Equal::make(input),
        '+' => Plus::make(input),
        '<' => LessThan::make(input),

        c if c.is_alphabetic() || c == '_' => Ident::make(input),

        c => Err(LexError::UnknownStart(c)),
    })
}

fn whitespaces_opt(input: &str) -> &str {
    input
        .char_indices()
        .find(|(_, c)| !c.is_whitespace())
        .map(|(idx, _)| input.split_at(idx).1)
        .unwrap_or("")
}

fn first_char(input: &str) -> Option<char> {
    input.chars().next()
}

type LexingResult<'input> = Result<(&'input str, Token<'input>), LexError>;

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum LexError {
    UnknownStart(char),
}

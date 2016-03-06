extern crate rustc_serialize;

use std::io::{self,Read,Write};
use std::mem::swap;
use rustc_serialize::json as json;

pub struct Prettifier {
    /// Current token from the JSON parser
    token: Option<json::JsonEvent>,
}

fn spaces(wr: &mut io::Write, n: u32) -> io::Result<()> {
    let mut n = n as usize;
    const BUF: &'static [u8] = b"                ";

    while n >= BUF.len() {
        try!(wr.write(BUF));
        n -= BUF.len();
    }

    if n > 0 {
        try!(wr.write(&BUF[..n]));
    }
    Ok(())
}

pub fn escape_bytes(wr: &mut io::Write, bytes: &[u8]) -> io::Result<()> {
    try!(wr.write_all(b"\""));

    let mut start = 0;

    for (i, byte) in bytes.iter().enumerate() {
        let escaped = match *byte {
            b'"' => b"\\\"",
            b'\\' => b"\\\\",
            b'\x08' => b"\\b",
            b'\x0c' => b"\\f",
            b'\n' => b"\\n",
            b'\r' => b"\\r",
            b'\t' => b"\\t",
            _ => { continue; }
        };

        if start < i {
            try!(wr.write_all(&bytes[start..i]));
        }

        try!(wr.write_all(escaped));

        start = i + 1;
    }

    if start != bytes.len() {
        try!(wr.write_all(&bytes[start..]));
    }

    try!(wr.write_all(b"\""));
    Ok(())
}

/// Escapes the given string into the writer.
pub fn escape_str(wr: &mut io::Write, value: &str) -> io::Result<()> {
    escape_bytes(wr, value.as_bytes())
}

impl Prettifier {
    pub fn new() -> Prettifier {
        Prettifier { token: None, }
    }

    pub fn stream(&mut self, src: &mut io::Read, dest: &mut io::Write) -> Result<(), json::BuilderError> {
        let mut parser = json::Parser::new(src.bytes().map(|c| c.unwrap() as char));
        self.bump(parser.next());
        let result = self.build_value(&mut parser, dest, 0);
        self.bump(parser.next());
        match self.token.take() {
            None => {}
            Some(json::JsonEvent::Error(e)) => { return Err(e); }
            _ => {
                return Err(
                    json::ParserError::SyntaxError(
                        json::ErrorCode::InvalidSyntax, 0, 0)); }
        }
        result
    }

    fn bump(&mut self, token: Option<json::JsonEvent>) {
        self.token = token;
    }

    fn build_value<T>(&mut self, parser: &mut json::Parser<T>, dest: &mut io::Write, curr_indent: u32)
                   -> Result<(), json::BuilderError>
        where T: Iterator<Item = char>
    {
        return match self.token.take() {
            Some(json::JsonEvent::NullValue) => {try!(write!(dest, "null")); Ok(())},
            Some(json::JsonEvent::I64Value(n)) => {try!(write!(dest, "{}", n)); Ok(())},
            Some(json::JsonEvent::U64Value(n)) => {try!(write!(dest, "{}", n)); Ok(())},
            Some(json::JsonEvent::F64Value(n)) => {try!(write!(dest, "{}", n)); Ok(())},
            Some(json::JsonEvent::BooleanValue(b)) => {
                if b { try!(write!(dest, "true")) }
                else { try!(write!(dest, "false"))};
                Ok(())},
            Some(json::JsonEvent::StringValue(ref mut s)) => {
                let mut temp = String::new();
                swap(s, &mut temp);
                try!(escape_str(dest, &temp));
                Ok(())
            }
            Some(json::JsonEvent::ArrayStart) => self.build_array(parser, dest, curr_indent),
            Some(json::JsonEvent::ObjectStart) => self.build_object(parser, dest, curr_indent),
            Some(json::JsonEvent::Error(e)) => Err(e),
            Some(json::JsonEvent::ObjectEnd) =>
                Err(json::ParserError::SyntaxError(
                    json::ErrorCode::InvalidSyntax, 0, 0)),
            Some(json::JsonEvent::ArrayEnd) =>
                Err(json::ParserError::SyntaxError(
                    json::ErrorCode::InvalidSyntax, 0, 0)),
            None =>
                Err(json::ParserError::SyntaxError(
                    json::ErrorCode::EOFWhileParsingValue, 0, 0)),
        }
    }

    fn build_array<T>(&mut self, parser: &mut json::Parser<T>, dest: &mut io::Write, old_indent: u32)
                   -> Result<(), json::BuilderError>
        where T: Iterator<Item = char>
    {
        let mut idx = 0;
        let curr_indent = old_indent + 2;
        self.bump(parser.next());
        if let Some(json::JsonEvent::ArrayEnd) = self.token {
            try!(write!(dest, "[]"));
            return Ok(());
        } else {
            try!(write!(dest, "["));
            loop {
                if idx != 0 {
                    try!(write!(dest, ","));
                }
                try!(write!(dest, "\n"));
                try!(spaces(dest, curr_indent));
                if let Err(e) = self.build_value(parser, dest, curr_indent) {
                    return Err(e);
                };

                self.bump(parser.next());
                idx += 1;

                if let Some(json::JsonEvent::ArrayEnd) = self.token {
                    try!(write!(dest, "\n"));
                    try!(spaces(dest, curr_indent - 2));
                    try!(write!(dest, "]"));
                    return Ok(());
                }

                if None == self.token {
                    return Err(
                        json::ParserError::SyntaxError(
                            json::ErrorCode::EOFWhileParsingArray, 0, 0));
                }
            }

        }
    }

    fn build_object<T>(&mut self, parser: &mut json::Parser<T>, dest: &mut io::Write, old_indent: u32)
                    -> Result<(), json::BuilderError>
        where T: Iterator<Item = char>
    {
        let mut idx = 0;
        self.bump(parser.next());
        match self.token.take() {
            Some(json::JsonEvent::ObjectEnd) => {
                try!(write!(dest, "{{}}"));
                return Ok(());
            }
            Some(json::JsonEvent::Error(e)) => { return Err(e); }
            None => {
                return Err(
                    json::ParserError::SyntaxError(
                        json::ErrorCode::EOFWhileParsingObject, 0, 0)); }
            token => { self.token = token; }
        }

        try!(write!(dest, "{{"));
        let curr_indent = old_indent + 2;
        loop {
            if idx != 0 {
                try!(write!(dest, ","));
            }
            try!(write!(dest, "\n"));
            try!(spaces(dest, curr_indent));
            // The token's we get from the stack don't include Object keys, we
            // have to get those directly from the stack
            let key = match parser.stack().top() {
                Some(json::StackElement::Key(k)) => { k.to_string() }
                _ => { panic!("invalid state"); }
            };
            try!(write!(dest, "{:?}: ",key));
            if let Err(e) = self.build_value(parser, dest, curr_indent) {
                return Err(e);
            };
            self.bump(parser.next());
            idx += 1;

            match self.token.take() {
                Some(json::JsonEvent::ObjectEnd) => {
                    try!(write!(dest, "\n"));
                    try!(spaces(dest, curr_indent - 2));
                    try!(write!(dest, "}}"));
                    return Ok(());
                }
                Some(json::JsonEvent::Error(e)) => { return Err(e); }
                None => {
                    return Err(
                        json::ParserError::SyntaxError(
                            json::ErrorCode::EOFWhileParsingObject, 0, 0)); }
                token => { self.token = token; }
            }
        }
    }
}

/// Prettifies the JSON content from the reader into the writer.
/// # Examples
///
/// ```
/// # extern crate beautician;
/// # extern crate rustc_serialize;
///
/// # fn foo() -> Result<(), rustc_serialize::json::BuilderError> {
/// let mut reader: &[u8] = b"[\"hello\"]";
/// let mut writer: Vec<u8> = vec![];
///
/// try!(beautician::prettify(&mut reader, &mut writer));
///
/// assert_eq!(b"[\n  \"hello\"\n]", &writer[..]);
/// # Ok(())
/// # }
/// ```
pub fn prettify(src: &mut io::Read, dest: &mut io::Write)
            -> Result<(), json::BuilderError> {
    Prettifier::new().stream(src, dest)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn it_works() {
        let mut reader: &[u8] = b"[\"hello\"]";
        let mut writer: Vec<u8> = vec![];

        prettify(&mut reader, &mut writer).unwrap();

        assert_eq!(b"[\n  \"hello\"\n]", &writer[..]);
    }
}

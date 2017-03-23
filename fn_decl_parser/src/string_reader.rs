pub type BytePos = usize;

fn char_at(s: &str, offset: BytePos) -> char {
    s[offset..].chars().next().unwrap()
}

pub struct StringReader<'a> {
    pos: BytePos,
    input: &'a str,
}

impl<'a> StringReader<'a> {
    pub fn new(s: &'a str) -> StringReader<'a> {
        StringReader {
            pos: 0,
            input: s,
        }
    }

    pub fn pos(&self) -> BytePos { self.pos }
    
    pub fn next(&mut self) {
        let pos = self.pos;
        if pos < self.input.len() {
            let new_ch = char_at(self.input, pos);
            let new_ch_len = new_ch.len_utf8();
            self.pos += new_ch_len;
        }
    }

    pub fn peek(&self) -> Option<char> {
        if self.pos < self.input.len() {
            Some(char_at(self.input, self.pos))
        } else {
            None
        }
    }

    pub fn advance_while<P>(&mut self, pred: P) where P: Fn(char) -> bool {
        while let Some(c) = self.peek() {
            if !pred(c) {
                return;
            }
            self.next();
        }
    }
    
    pub fn str_from(&self, start: BytePos) -> &'a str {
        &self.input[start..self.pos]
    }

    pub fn str_from_to(&self, start: BytePos, end: BytePos) -> &'a str {
        &self.input[start..end]
    }
}

#[test]
fn test_string_reader() {
    let str = "fn blah(foo: i32) -> crap;";
    let mut reader = StringReader::new(str);
    assert_eq!(reader.peek(), Some('f')); reader.next();
    assert_eq!(reader.peek(), Some('n'));
}

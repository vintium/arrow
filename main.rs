#![feature(slice_group_by)]
use std::convert::TryFrom;
use std::fs;
use std::io;

/*
#[derive(Debug)]
struct Node {
    value: Token,
    children: Vec<Box<Node>>,
}

impl Node {
    fn new() -> Node {
        Node {
            value: String::new(),
            children: Vec::with_capacity(6),
        }
    }

    fn with(val: &str) -> Node {
        let mut res = Node::new();
        res.value = String::from(val);
        res
    }

    fn attach_child(&mut self, child: Node) {
        assert!(self.children.len() <= 5);
        self.children.push(Box::new(child));
    }
}
*/

/*
fn validate(toks: &Vec<&str>) -> bool {
   let mut res: bool = true;
   /* find problems */
   if Some(&ARROW_RIGHT)    == toks.get(0)
      || Some(&ARROW_LEFT)  == toks.get(0)
      || Some(&ARROW_RIGHT) == toks.get(toks.len() - 1)
      || Some(&ARROW_LEFT)  == toks.get(toks.len() - 1) {
      eprintln!("ERROR: Cannot direct to/from nothing.");
      res = false;
   }

   let mut i: usize = 0;
   for _ in toks.iter() {
      /* disallow direction to multiple places (for now.)
         TODO: when multiple return values are introduced,
         this will need to be changed.
      */
      if Some(&ARROW_LEFT) == toks.get(i.checked_sub(1).unwrap_or(0))
         && Some(&ARROW_RIGHT) == toks.get(i + 1) {
         /* we have some kind of expression like `add <- 5 -> print` */
         eprintln!("ERROR: Cannot direct a value to multiple places.");
         res = false;
      }
      i += 1;
   }
   res
}
*/

#[derive(Debug, Clone)]
enum Arrow {
    /* singular arrows */
    Right,
    Left,
    Up,
    Down,

    /* combinations of two arrows */
    VerticalRight,
    UpRight,
    DownRight,
    VerticalLeft,
    UpLeft,
    DownLeft,

    /* combinations of three arrows */
    ThreeRight,
    ThreeLeft,
}

impl Arrow {
    /* singular arrows */
    const RIGHT: &'static str = "->";
    const LEFT: &'static str = "<-";
    const UP: &'static str = "|^";
    const DOWN: &'static str = "|v";

    /* combinations of two arrows */
    const VERTICAL_RIGHT: &'static str = "|>";
    const UP_RIGHT: &'static str = "\\>";
    const DOWN_RIGHT: &'static str = "/>";
    const VERTICAL_LEFT: &'static str = "<|";
    const UP_LEFT: &'static str = "</";
    const DOWN_LEFT: &'static str = "<\\";

    /* combinations of three arrows */
    const THREE_RIGHT: &'static str = "-|>";
    const THREE_LEFT: &'static str = "<|-";
}

use std::str::FromStr;

impl FromStr for Arrow {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            Self::RIGHT => Ok(Self::Right),
            Self::LEFT => Ok(Self::Left),
            Self::UP => Ok(Self::Up),
            Self::DOWN => Ok(Self::Down),
            Self::VERTICAL_RIGHT => Ok(Self::VerticalRight),
            Self::UP_RIGHT => Ok(Self::UpRight),
            Self::DOWN_RIGHT => Ok(Self::DownRight),
            Self::VERTICAL_LEFT => Ok(Self::VerticalLeft),
            Self::UP_LEFT => Ok(Self::UpLeft),
            Self::DOWN_LEFT => Ok(Self::DownLeft),
            Self::THREE_RIGHT => Ok(Self::ThreeRight),
            Self::THREE_LEFT => Ok(Self::ThreeLeft),
            other => Err(format!("Could not convert {} into an `Arrow`.", other)),
        }
    }
}

/*
fn find_center(toks: &Vec<Vec<String>>) -> (usize, usize) {
   /* find root node */
   let mut i: usize = 0;
   let mut j: usize = 0;
   for l in toks.iter() {
      i = 0;
      for t in l.iter() {
         let token_below = toks.get(j + 1).and_then(|x| x.get(i)).map(|x| &**x);
         if Some(ARROW_RIGHT) == l.get(i + 1).map(|x| &**x) {
            /* pass */
         }
         if Some(ARROW_DOWN) == token_below {
            println!("went down a row");
            /* go down a row */
            break;
         }
         if Some(ARROW_LEFT) == l.get(i + 1).map(|x| &**x) {
            return (i, j);
         }
         if Some(ARROW_UP) == toks.get(j + 1).and_then(|x| x.get(i)).map(|x| &**x) {
            return (i, j);
         }
         println!("{:?}", token_below);
         i += 1;
      }
      j += 1;
   }
   println!("tail return");
   return (i - 1, j - 1);
}

fn build_ast(node: &mut Node, toks: &Vec<&str>, center: usize) {
   node.value = toks[center].to_string();
   if toks.get(center.checked_sub(1).unwrap_or(0)) == Some(&ARROW_RIGHT) {
      node.attach_child(Node::new());
      let l = node.children.len();
      build_ast(&mut node.children[l - 1], toks, center - 2);
   }
   if toks.get(center + 1) == Some(&ARROW_LEFT) {
      node.attach_child(Node::new());
      let l = node.children.len();
      build_ast(&mut node.children[l - 1], toks, center + 2);
   }
}
*/

#[derive(Debug, Clone)]
struct Loc {
    row: usize,
    col: usize,
    len: usize,
}

impl Loc {
    fn new() -> Self {
        Loc {
            row: 0,
            col: 0,
            len: 0,
        }
    }

    fn is_above(&self, other: &Self) -> bool {
        self.row < other.row &&
        self.col <= (other.col + other.len) &&
        (self.col + self.len) >= other.col
    }

    fn is_rightward(&self, other: &Self) -> bool {
        self.row == other.row &&
        self.col > other.col
    }

    fn is_leftward(&self, other: &Self) -> bool {
        self.row == other.row &&
        self.col < other.col
    }

    fn is_beneath(&self, other: &Self) -> bool {
        self.row > other.row &&
        self.col <= (other.col + other.len) &&
        (self.col + self.len) >= other.col
    }
}

#[derive(Debug, Clone)]
enum TokenValue {
    Arrow(Arrow),
    Numlit(i64),
    Strlit(String),
    Word,
}

#[derive(Debug, Clone)]
struct Token {
    val: TokenValue,
    text: String,
    loc: Loc,
}

struct Adjacent {
    top: Option<Token>,
    bottom: Option<Token>,
    left: Option<Token>,
    right: Option<Token>,
}

struct Tokenizer {
    row: usize,
    col: usize,
    rest: String,
}

impl Tokenizer {
    fn new(source: String) -> Self {
        Tokenizer {
            row: 0,
            col: 0,
            rest: source,
        }
    }

    fn trim_start(&mut self) {
        // if self.rest starts with something like "   \n \n  69 ->", we need to
        // first get rid of the "   \n \n" and appropriately adjust self.row to
        // indicate that we've traveled down some rows.

        // if str::trim_start() were better and returned the part it trimmed along with the rest,
        // we could do this by just counting the amount of
        // newlines in the part of the string that was trimmed, but alas.
        // TODO: just have realized that we can get the part that trim_start() trimmed. reimplement this
        // using that method.
        while let Some((prt, _)) = self.rest.split_once('\n') {
            if prt.trim() != "" {
                break;
            }
            self.row += 1;
            self.col = 0;
            let l = prt.len();
            self.rest.drain(..(l + 1));
        }
        // following the example at the top, at this point the beginning of self.rest would look
        // something like "  69 ->". We need to remove the whitespace and appropriately adjust
        // self.col to indicate that we've traveled over some columns.
        let no_leading_whitespace = self.rest.trim_start();
        let amt_trimmed = self.rest.len() - no_leading_whitespace.len();
        self.col += amt_trimmed;
        self.rest = String::from(no_leading_whitespace);
    }

    fn parse_string(&self) -> Option<(&str, TokenValue)> {
        let quote_idxs: Vec<usize> = self.rest.match_indices('"').map(|(idx, _)| idx).collect();
        let mut str_ends: Option<usize> = None;
        if quote_idxs.len() >= 2 && quote_idxs[0] == 0 {
            for idx in &quote_idxs[1..] {
                if let Some('\\') = self.rest.chars().nth(idx - 1) {
                    continue;
                } else {
                    str_ends = Some(*idx);
                    break;
                }
            }
        }
        if let Some(end) = str_ends {
            // `end` and 0 are guaranteed to be valid indices, so it's safe to `.unwrap()`
            let text = self.rest.get(0..=end).unwrap();
            let mut val = String::from(text);
            assert_eq!(val.remove(0), '"');
            assert_eq!(val.pop().unwrap(), '"');
            val = val.replace(r"\n", "\n");
            val = val.replace(r#"\""#, "\"");
            val = val.replace(r"\\", "\\");
            // TODO: Fail here if there are invalid escape sequences instead of ignoring them !!!!
            Some((text, TokenValue::Strlit(val)))
        } else {
            None
        }
    }

    fn parse_number(&self) -> Option<(&str, TokenValue)> {
        let text = self.rest.split_once(char::is_whitespace)?.0;
        let radix;
        if text.starts_with("0x") {
            radix = 16;
        } else if text.starts_with("0b") {
            radix = 2;
        } else if text.starts_with("0o") {
            radix = 8;
        } else {
            radix = 10;
        }
        Some((
            text,
            TokenValue::Numlit(
                i64::from_str_radix(
                    if radix != 10 {
                        text.split_at(2).1
                    } else {
                        text
                    },
                    radix,
                )
                .ok()?,
            ),
        ))
    }

    fn parse_arrow(&self) -> Option<(&str, TokenValue)> {
        let text = self.rest.split_once(char::is_whitespace)?.0;
        Some((text, TokenValue::Arrow(Arrow::from_str(text).ok()?)))
    }

    fn parse_word(&self) -> Option<(&str, TokenValue)> {
        return Some((
            self.rest.split_once(char::is_whitespace)?.0,
            TokenValue::Word,
        ));
    }
}

use std::io::Read;
// TODO: make this generic for T where T: io::Read
impl TryFrom<fs::File> for Tokenizer {
    type Error = io::Error;

    fn try_from(mut reader: fs::File) -> io::Result<Self> {
        let mut buf = String::new();
        reader.read_to_string(&mut buf)?;
        Ok(Tokenizer::new(buf))
    }
}

#[derive(Debug)]
enum TokenizerError {
    DissallowedCharacter,
}

impl Iterator for Tokenizer {
    type Item = Result<Token, TokenizerError>;
    fn next(&mut self) -> Option<Self::Item> {
        self.trim_start();
        if let Some((current, tokval)) = self.parse_string()
            .or(self.parse_arrow())
            .or(self.parse_number())
            .or(self.parse_word()) {
            let (val, text, len) = (tokval, String::from(current), current.len());
            let (row, col) = (self.row, self.col);
            self.rest.drain(..len);
            self.col += len;
            Some(Ok(Token {
                val,
                text,
                loc: Loc { row, col, len },
            }))
        } else {
            None
        }
    }
}


/*
enum Facing {
    In,
    Out,
}
*/


#[derive(Debug)]
struct TokenGrid(Vec<Vec<Token>>);

impl From<Tokenizer> for TokenGrid {
    fn from(t: Tokenizer) -> Self {
        let mut toks: Vec<Token> = t.map(Result::unwrap).collect();
        let mut grid: Vec<Vec<Token>> = toks.group_by_mut(|a, b| a.loc.row == b.loc.row)
            .map(|a| { a.sort_by_key(|b| b.loc.col); a.to_vec() })
            .collect();
        grid.sort_by_key(|a| a[0].loc.row);
        TokenGrid(grid)
    }

}

use std::ops::Index;
impl Index<(usize, usize)> for TokenGrid {
    type Output = Token;
    fn index(&self, pos: (usize, usize)) -> &Self::Output {
        self.get(pos).unwrap()
    }
}

impl TokenGrid {
    fn get(&self, pos: (usize, usize)) -> Option<&Token> {
        self.0.get(pos.0)?.get(pos.1)
    }

    fn get_above(&self, mut pos: (usize, usize)) -> Option<&Token> {
        assert!(self.get(pos).is_some()); 
        let original = &self[pos];
        while pos.0 > 0 {
            pos.0 -= 1;
            let row = self.0.get(pos.0)?;
            for t in row {
                if t.loc.is_above(&original.loc) { return Some(t) }
            }
        }
        None
    }


}


fn main() -> io::Result<()> {
    // let mut tokens: Vec<Vec<String>> = Vec::new();
    /*
    for line in io::BufReader::new(fs::File::open("./test.arr")?).lines() {
       let mut root_node: Node = Node::new();
       let line = line?;
       let line_tokens: Vec<String> = line.split(' ')
          .filter(|a| !a.is_empty())
          .map(|a| a.to_string())
          .collect();
       tokens.push(line_tokens);
    }
    */
    let toks = Tokenizer::try_from(fs::File::open("./test.arr")?)?;
    /* let grid: Vec<Vec<Token>> = { 
        let mut grid_mut: Vec<Vec<Token>> = toks.group_by(|a, b| a.loc.row == b.loc.row)
        .map(|a| { let mut row = a.to_vec(); row.sort_by_key(|a| a.loc.col); row})
        .collect();
        grid_mut.sort_by_key(|a| a[0].loc.row);
        grid_mut
    };
    */
    let grid = TokenGrid::from(toks);
    let atok = &grid[(1, 0)];
    let fooby = grid.get_above((1, 0));
    println!("{:?}", fooby);

    // let c = find_center(&tokens);
    // println!("{:?}", tokens);
    // println!("{:?}", c);
    // println!("{:?}", tokens[c.1][c.0]);
    Ok(())
}

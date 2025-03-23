use {
  std::fs,
  crate::{shell::builtins::ShellBuiltIns, jobs::*}
};


#[derive(Debug,Clone,PartialEq)]
pub enum Token {
  File(String),           // cd, who, ./file.sh
  String(String),         // "hello" 'what'       // might impl a char
  Pipe,                   // |
  Redirection,            // >
  AppendableRedirection,  // >>
  Null,                   // ?
  Enter,                  // \n

  Semicolon,              // ;
  And,                    // &&
}

#[derive(Debug,PartialEq)]
pub enum Node {
  And(Box<Node>, Box<Node>),
  Pipe(Box<Node>, Box<Node>),
  Redirect(Box<Node>, Box<Node>),
  RedirectAppend(Box<Node>, Box<Node>),
  String(String, Vec<Box<Node>>),
  Enter,
  Nullus
}

pub enum SHFructa {
  POPEN(String),
  ProcExit,
  SigExit,

  Raw(String),
}

#[derive(Debug,Clone)]
pub enum EvalType {
  Raw,
  Direct,
  Redirected,
}




pub trait SHLanguage {
  fn tokenize(&self, input: String) -> Vec<Token>;
  fn parse(&self, tokens: Vec<Token>) -> Vec<Node>;

  fn parse_and(&self, tokens: &mut Vec<Token>, deep: bool) -> Node;
  fn parse_redirect(&self, tokens: &mut Vec<Token>, deep: bool) -> Node;
  fn parse_pipe(&self, tokens: &mut Vec<Token>, deep: bool) -> Node;
  // add !(), {} somewhere
  fn parse_primary(&self, tokens: &mut Vec<Token>, deep: bool) -> Node;

  fn eat(&self, tokens: &mut Vec<Token>) -> Token;
  fn is_alpha(&self, chr: char) -> bool;

  fn evaluate(&mut self, node: Node, eval: EvalType) -> SHFructa;
  fn evaluate_program(&mut self, nodes: Vec<Node>) -> SHFructa;
  fn evaluate_string(&mut self, node: Node, eval: EvalType) -> SHFructa;
  fn evaluate_redirect(&mut self, node: Node, eval: EvalType) -> SHFructa;
  fn evaluate_pipe(&mut self, node: Node, eval: EvalType) -> SHFructa;

  fn evaluate_and(&mut self, node: Node, eval: EvalType) -> SHFructa;
}



impl SHLanguage for crate::Shell {
  fn is_alpha(&self, chr: char) -> bool {
    chr.is_alphanumeric() || ['.', '/'].contains(&chr)
  }
  fn tokenize(&self, input: String) -> Vec<Token> {
    let mut result = vec![];
    let mut input = input.chars().collect::<Vec<char>>();
    let mut delete;
    while input.len() > 0 {
      delete = true;
      result.push(match input[0] {
        '"' => {
          input.remove(0);
          let mut buffer = String::new();
          while input.len() > 0 && input[0]!='"' {
            buffer += &input[0].to_string();
            input.remove(0);
          }
          if input.len() == 0 {
            todo!()
          }
          Token::String(buffer)
        },
        '|' => Token::Pipe,
        ';' => Token::Semicolon,
        '&' => {
          match input[1] {
            '&' => {input.remove(0); Token::And},
            _ => Token::Null,
          }
        }
        '>' => {
          match input[1] {
            '>' => {input.remove(0); Token::AppendableRedirection},
            _ => Token::Redirection,
          }
        },
        '\n' => Token::Enter,
        _ => {
          if self.is_alpha(input[0]) {
            delete = false;
            let mut buffer = String::new();
            while input.len()>0 && self.is_alpha(input[0]) {
              buffer += &input[0].to_string();
              input.remove(0);
            }
            Token::File(buffer)
          } else {
            Token::Null
          }
        },
      });
      if delete {
        input.remove(0);
      }
    }
    result.iter().filter(|x| x!=&&Token::Null).collect::<Vec<&Token>>().iter().map(|x| x.clone().to_owned()).collect::<Vec<Token>>()
  }

  fn eat(&self, tokens: &mut Vec<Token>) -> Token {
    tokens.remove(0)
  }

  fn parse(&self, tokens: Vec<Token>) -> Vec<Node> {
    let mut nodes: Vec<Node> = vec![];
    let mut tokens = tokens;

    while tokens.len() > 0  {
      nodes.push(self.parse_and(&mut tokens, true));
    }
    nodes
  }
  fn parse_and(&self, tokens: &mut Vec<Token>, deep: bool) -> Node {
    let mut left = self.parse_redirect(tokens, deep);

    if tokens.len() > 0 {
      match tokens[0] {
        Token::And => {
          self.eat(tokens);
          left = Node::And(
            Box::new(left), 
            Box::new(self.parse_redirect(tokens, deep))
          )
        }
        _ => {}
      }
    }
    left
  }

  fn parse_redirect(&self, tokens: &mut Vec<Token>, deep: bool) -> Node {
    let mut left = self.parse_pipe(tokens, deep);

    if tokens.len() > 0 {
      match tokens[0] {
        Token::Redirection => {
          self.eat(tokens);
          left = Node::Redirect(
            Box::new(left), 
            Box::new(self.parse_pipe(tokens, deep))
          )
        },
        Token::AppendableRedirection => {
          self.eat(tokens);
          left = Node::RedirectAppend(
            Box::new(left), 
            Box::new(self.parse_pipe(tokens, deep))
          )
        },
        _ => {}
      }
    }
    left
  }
  fn parse_pipe(&self, tokens: &mut Vec<Token>, deep: bool) -> Node {
    let mut left = self.parse_primary(tokens, deep);

    if tokens.len() > 0 {
      match tokens[0] {
        Token::Pipe => {
          self.eat(tokens);
          left = Node::Pipe(
            Box::new(left), 
            Box::new(self.parse_primary(tokens, deep))
          )
        }
        _ => {}
      }
    }
    left
  }

  fn parse_primary(&self, tokens: &mut Vec<Token>, deep: bool) -> Node {
    let eat = self.eat(tokens);
    match eat {
      Token::File(i) => {
        let mut children: Vec<Box<Node>> = vec![];
        if deep {
          while tokens.len()>0 && match tokens[0] {Token::String(_) | Token::File(_) => true, _ => false} {
            children.push(Box::new(self.parse_primary(tokens, false)));
          }
        }
        Node::String(i, children)
      },
      Token::String(i) => {
        let mut children: Vec<Box<Node>> = vec![];
        if deep {
          while tokens.len()>0 && match tokens[0] {Token::String(_) | Token::File(_) => true, _ => false} {
            children.push(Box::new(self.parse_primary(tokens, false)));
          }
        }
        Node::String(i, children)
      },
      Token::Enter => Node::Enter,
      _ => Node::Nullus
    }
  }

  fn evaluate_program(&mut self, nodes: Vec<Node>) -> SHFructa {
    let mut last_eval = SHFructa::ProcExit;
    for node in nodes {
      if node != Node::Enter {
        last_eval = self.evaluate(node, EvalType::Direct);
      }
    }
    last_eval
  }
  fn evaluate(&mut self, node: Node, eval: EvalType) -> SHFructa {
    match node {
      Node::String(..) => self.evaluate_string(node, eval),
      Node::Redirect(..) => self.evaluate_redirect(node, eval),
      Node::RedirectAppend(..)  => self.evaluate_redirect(node, eval),
      Node::And(..) => self.evaluate_and(node, eval),
      _ => todo!()
    }
  }

  fn evaluate_and(&mut self, node: Node, eval: EvalType) -> SHFructa {
    match node {
      Node::And(left, right) => {
        let left_evaluate = self.evaluate(*left, eval.clone());

        match left_evaluate {
          SHFructa::SigExit => left_evaluate,
          _ => self.evaluate(*right, eval)
        }

      },
      _ => panic!()
    }
  }

  fn evaluate_pipe(&mut self, node: Node, eval: EvalType) -> SHFructa {
    match node {
      Node::Pipe(from, to) => {
        let from = match self.evaluate(*from, EvalType::Redirected) {
          SHFructa::POPEN(s) => s,
          _ => panic!()
        };

        todo!();

        SHFructa::ProcExit
      }
      _ => todo!()
    }
  }

  fn evaluate_redirect(&mut self, node: Node, eval: EvalType) -> SHFructa {
    match node {
      Node::Redirect(from, to) => {
        let from = match self.evaluate(*from, EvalType::Redirected) {
          SHFructa::POPEN(s) => s,
          _ => panic!()
        };
        let to = match self.evaluate(*to, EvalType::Raw) {
          SHFructa::Raw(s) => s,
          _ => panic!()
        };
        let _ = fs::write(to, from).unwrap();
        SHFructa::ProcExit
      }
      Node::RedirectAppend(from, to) => {
        let from = match self.evaluate(*from, EvalType::Redirected) {
          SHFructa::POPEN(s) => s,
          _ => panic!()
        };
        let to = match self.evaluate(*to, EvalType::Raw) {
          SHFructa::Raw(s) => s,
          _ => panic!()
        };
        let file_contents = match fs::read_to_string(&to) {
          Ok(s) => s,
          Err(_) => String::new(),
        };
        let _ = fs::write(to, file_contents + "\n" + &from).unwrap();
        SHFructa::ProcExit
      }
      _ => panic!()
    }
  }
  fn evaluate_string(&mut self, node: Node, eval: EvalType) -> SHFructa {
    match node {
      Node::String(i, children) => {
        match eval {
          EvalType::Direct => {
            let mut tokens: Vec<String> = vec![];
            tokens.push(i);
            for x in children {
              let val = match *x {
                Node::String(s, c) => {
                  if c.len() > 0 {
                    todo!()
                  }
                  s
                },
                _ => todo!()
              }.clone();
              tokens.push(val);
            }
            match (&tokens[0]) as &str {
              "cd" => {println!("{:#?}", self.cd(tokens[1].to_string()))},
              "exit" => {return SHFructa::SigExit;},
              "clear"  => {self.clear();},
              _ => spawn_proc(tokens.iter().map(|x| x as &str).collect::<Vec<&str>>()),
            }
            SHFructa::ProcExit
          },
          EvalType::Redirected => {
            let mut tokens: Vec<String> = vec![];
            tokens.push(i);
            for x in children {
              let val = match self.evaluate_string(*x, EvalType::Raw) {
                SHFructa::Raw(s) => s,
                _ => panic!()
              }.clone();
              tokens.push(val);
            }
            SHFructa::POPEN(spawn_proc_output(tokens.iter().map(|x| x as &str).collect::<Vec<&str>>()))
          },
          EvalType::Raw => {
            if children.len() > 0 {
              todo!()
            }
            SHFructa::Raw(i)
          },
        }
     }
      _ => todo!()
    }
  }
}

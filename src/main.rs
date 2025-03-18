use {
  libc::{self, execvp}, 
  std::{env, ffi::CString, fmt::Display, fs::{self, read_to_string}, io::{self, Read, Write}, path::{Path, PathBuf}, process::Command, ptr::null, str, sync::Mutex}
};

fn spawn_proc_output(args: Vec<&str>) -> String {
  str::from_utf8(&Command::new(args[0]).args(args[1..].iter()).output().unwrap().stdout).unwrap().to_string()
}

fn spawn_proc(args: Vec<&str>) {
  let p_id = unsafe { libc::fork() }; // fork the current proc

  /* now, both the parent and the child will execute the following */

  if p_id == 0 {  /* this implies, it's the child proc */
    //unsafe{println!("### Child ###\nCurrent PID: {} and Child PID: {}\n", libc::getpid(), p_id)};
    //let args = args.into_iter().map(|x| CString::new(x).unwrap().as_ptr()).collect::<Vec<*const libc::c_char>>();
    
    unsafe {
      libc::signal(libc::SIGINT, libc::SIG_DFL);
    }

    let mut argsv: Vec<CString> = vec![];//args.clone().into_iter().map(|x| {let a = CString::new(x).unwrap(); a.as_ptr()}).collect::<Vec<*const libc::c_char>>();
    for i in &args {
      argsv.push(CString::new(*i).unwrap());
    }
    let mut argsv2 = vec![];

    for i in &argsv { argsv2.push(i.as_ptr()); }
    argsv2.push(null());
    
    let argv = argsv2.as_ptr();
    
    let command = CString::new(args[0]).unwrap();
    unsafe{execvp(command.as_ptr(), argv);}

  } else {
    let mut stat_loc = 1;
    unsafe {
      let wait = libc::waitpid(p_id, &mut stat_loc, libc::WUNTRACED); // Sleep for one second
      /*println!("### Parent ###\nCurrent PID: {} and Child PID: {}\n",
        libc::getpid(), p_id);*/
    }
  }
}

enum Fructa {
  Exit,
  Pass,
}

trait ShellBuiltIns {
  fn cd(&mut self, path: String) -> i32;
  fn run(&mut self) -> Fructa;
  fn clear(&self);
  fn clear_line(&self);
  fn redraw(&self);
  fn ctrl_c(&mut self);

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

  fn evaluate_and(&mut self, node: Node, eval: EvalType) -> SHFructa;
}

#[derive(Debug,Clone,PartialEq)]
enum Token {
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
enum Node {
  And(Box<Node>, Box<Node>),
  Pipe(Box<Node>, Box<Node>),
  Redirect(Box<Node>, Box<Node>),
  RedirectAppend(Box<Node>, Box<Node>),
  String(String, Vec<Box<Node>>),
  Enter,
  Nullus
}

enum SHFructa {
  POPEN(String),
  ProcExit,
  SigExit,

  Raw(String),
}

#[derive(Debug,Clone)]
enum EvalType {
  Raw,
  Direct,
  Redirected,
}



#[derive(Clone)]
struct Shell {
  input: String,
  state: State,
  cursor: u16,
}
#[derive(Clone)]
enum State {
  Normal,
  RSearch,
}


impl ShellBuiltIns for Shell {
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
  


  fn ctrl_c(&mut self) {
    println!("^C");
    self.input = String::new();
    self.cursor = 0;
    self.redraw();
  }
  fn clear(&self) {
    println!("\x1b[2J\x1b[H");
    io::stdout().flush().unwrap();
  }
  fn clear_line(&self) {
    print!("\x1b[2K");
    io::stdout().flush().unwrap();
  }
  fn redraw(&self) {
    self.clear_line();
    let mut combined_string = String::new();
    combined_string += &format!("\x1b[1E\x1b[1F");
    match self.state {
      State::Normal => {
        combined_string += &format!("{}", self);
      },
      State::RSearch => {
        combined_string += &format!("(rsearch)`':")
      },
    }
    combined_string += " ";
    combined_string += &self.input;

    if self.input.len()>0 { combined_string += &format!("\x1b[{}D", self.input.len()); };
    if self.cursor>0 { combined_string += &format!("\x1b[{}C", self.cursor); }


    print!("{}", combined_string);
    io::stdout().flush().unwrap();
  }
  fn run(&mut self) -> Fructa {
    if self.input.is_empty() {
      return Fructa::Exit;
    }

    let tokens = self.tokenize(self.input.clone());
    //println!("TOKENS: {:#?}", tokens);
    let nodes = self.parse(tokens);
    //println!("NODES: {:#?}", nodes);
    self.evaluate_program(nodes);
    println!();
    /*match tokens[0] {
      "cd" => {println!("{:#?}", self.cd(tokens[1].to_string()))},
      "exit" => {return Fructa::Exit;},
      "clear"  => {self.clear();},
      _ => spawn_proc(tokens),
    }*/
    self.input = String::new();
    self.cursor = 0;
    Fructa::Pass
  }
  fn cd(&mut self, path: String) -> i32 {
    let path = CString::new(path).unwrap();
    unsafe {libc::chdir(path.as_ptr()) }
  }
}

impl Display for Shell {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let hostname = match &env::var("HOSTNAME") {
      Ok(s) => s.clone(),
      Err(_) => String::from("???"),
    };
    let user = match &env::var("USER") {
      Ok(s) => s.clone(),
      Err(_) => String::from("???"),
    };
    let dir = str::from_utf8(&Command::new("pwd").output().unwrap().stdout).unwrap().trim().replace(&env::var("HOME").unwrap(), "~");
    write!(f, "[{user}@{hostname}:{dir}]$")
  }
}





static termios: Mutex<libc::termios> = Mutex::new(libc::termios { c_iflag: 0, c_oflag: 0, c_cflag: 0, c_lflag: 0, c_line: 1, c_cc: [0 as u8; 32], c_ispeed: 1, c_ospeed: 1 });



fn setup_termios() {
  termios.lock().unwrap().c_cflag &= !libc::CSIZE;
  termios.lock().unwrap().c_cflag |= libc::CS8;
  termios.lock().unwrap().c_cc[libc::VMIN] = 1;
}

extern "C" fn disable_raw_mode() {
  unsafe {
    libc::tcsetattr(libc::STDIN_FILENO, libc::TCSAFLUSH, &(*termios.lock().unwrap()));
  }
}
fn enable_raw_mode() {
  unsafe {
    libc::tcgetattr(libc::STDIN_FILENO, &mut *termios.lock().unwrap());
    libc::atexit(disable_raw_mode);
    let mut raw = *termios.lock().unwrap();
    raw.c_lflag &= !(libc::ECHO | libc::ICANON);
    libc::tcsetattr(libc::STDIN_FILENO, libc::TCSAFLUSH, &raw);
  }
}


#[derive(Debug)]
pub struct KeyEvent {
  pub code: KeyCode,
  pub modifiers: Vec<Modifier>,
}
#[derive(Debug,PartialEq)]
pub enum Modifier {
  Control,
  Shift,
  //why even do it at this point
}
#[derive(Debug,PartialEq)]
pub enum Direction {
  Up,
  Down,
  Right,
  Left
}

#[derive(Debug,PartialEq)]
pub enum KeyCode {
  Escape,
  Enter,
  Backspace,
  Delete,
  Arrow(Direction),
  Char(char),
}

const ESCAPE: char = 27 as char;
const BACKSPACE: char = '\u{7f}';
const TAB: char = '\t';
const ENTER: char = '\n';

fn getch() -> char {
  io::stdin().bytes().next().unwrap().unwrap() as char
}

fn get_arrow() -> KeyCode {
  match getch() {'A' => KeyCode::Arrow(Direction::Up), 'B' => KeyCode::Arrow(Direction::Down), 'C' => KeyCode::Arrow(Direction::Right), 'D' => KeyCode::Arrow(Direction::Left),
                                                           _ => KeyCode::Escape }
}


struct SigHandler {shell: Shell}

impl SigHandler {}
static SIGHANDLER: Mutex<SigHandler> = Mutex::new(SigHandler {shell: Shell {input: String::new(), state: State::Normal, cursor: 0}});




extern fn sigint_handler(signo: libc::c_int) {
  let mut lock = SIGHANDLER.lock().unwrap();
  lock.shell.ctrl_c();
  drop(lock);
}

fn get_sighandler() -> libc::sighandler_t {
    sigint_handler as extern fn(libc::c_int) as *mut libc::c_void as libc::sighandler_t
}


fn main() {
  setup_termios();
  enable_raw_mode();
  println!();
  SIGHANDLER.lock().unwrap().shell = Shell {input: String::new(), state: State::Normal, cursor: 0};
  


  env::set_var("HOSTNAME", str::from_utf8(&Command::new("hostname").output().unwrap().stdout).unwrap().trim()); // this is cheating I think
  

  unsafe {
    libc::signal(libc::SIGINT, get_sighandler());
  }

  let NULL: *const i8 = null();
  let lock = SIGHANDLER.lock().unwrap();
  lock.shell.redraw();
  drop(lock);
  for c in io::stdin().bytes() {
    let c: char = c.unwrap() as char;
        
    let mut modifiers: Vec<Modifier> = vec![];
    if c.is_control() && ![ENTER, TAB, ESCAPE, BACKSPACE].contains(&c) {
      modifiers.push(Modifier::Control);
    }
    
    let event = KeyEvent{
      code: match c {BACKSPACE => KeyCode::Backspace, '\n' => KeyCode::Enter,
          
          'Ã' => {
            match getch() {
              '³' => KeyCode::Char('ó'),
              '\u{93}' => KeyCode::Char('Ó'),
              _ => KeyCode::Escape
            }
          },
          'Ä' => {
            match getch() {
              '\u{99}' => KeyCode::Char('ę'),
              '\u{98}' => KeyCode::Char('Ę'),
              '\u{87}' => KeyCode::Char('ć'),
              '\u{86}' => KeyCode::Char('Ć'),
              '\u{85}' => KeyCode::Char('ą'),
              '\u{84}' => KeyCode::Char('Ą'),
              _ => KeyCode::Escape
            }
          },
          'Å' => {
            match getch() {
              '\u{84}' => KeyCode::Char('ń'),
              '\u{83}' => KeyCode::Char('Ń'),
              '\u{82}' => KeyCode::Char('ł'),
              '\u{81}' => KeyCode::Char('Ł'),
              '\u{9b}' => KeyCode::Char('ś'),
              '\u{9a}' => KeyCode::Char('Ś'),
              'º' => KeyCode::Char('ź'),
              '¹' => KeyCode::Char('Ź'),
              '¼' => KeyCode::Char('ż'),
              '»' => KeyCode::Char('Ż'),
              _ => KeyCode::Escape
            }
          },
          '\u{1b}' => {
              match getch() {
                    '[' => {
                        match getch() {
                            'A' => KeyCode::Arrow(Direction::Up), 'B' => KeyCode::Arrow(Direction::Down), 'C' => KeyCode::Arrow(Direction::Right), 'D' => KeyCode::Arrow(Direction::Left),
                            '1' => match getch() {
                                ';' => match getch() 
                                { '5' => {modifiers.push(Modifier::Control); get_arrow()}, '2' => {modifiers.push(Modifier::Shift); get_arrow()}, _ => KeyCode::Escape}, _ => KeyCode::Escape
                            },
                            '3' => match getch() {
                              '~' => KeyCode::Delete,
                              _ => KeyCode::Escape,
                            }
                            _ => KeyCode::Escape,
                        }
                    },
                   _ => KeyCode::Escape}},
          _ => KeyCode::Char(c)},
      modifiers,
    };

    let mut lock = SIGHANDLER.lock().unwrap();
    
    match event.code {
      KeyCode::Enter => {
        lock.shell.input += "\n";
        println!();
        match lock.shell.run() {
          Fructa::Exit => break,
          _ => {lock.shell.redraw(); continue},
        };
      },
      KeyCode::Escape => {
        lock.shell.state = match lock.shell.state {
          State::RSearch => State::Normal,
          State::Normal => State::Normal,
        }
      },
      KeyCode::Char(c) => {
        match lock.shell.state {
          State::Normal => {
            if event.modifiers.contains(&Modifier::Control) {
              match c {
                '\u{12}' => {lock.shell.state = State::RSearch;lock.shell.redraw();},
                '\u{c}' => {lock.shell.clear();lock.shell.redraw();},
                '\u{4}' => {if lock.shell.input.is_empty() {break}},
                _ => {},
              }
            } else {
              lock.shell.cursor += 1;
              print!("{}", c);
              io::stdout().flush().unwrap();
              
              lock.shell.input += &c.to_string();
            }
          },
          State::RSearch => {
            if !event.modifiers.contains(&Modifier::Control) {
              print!("{}", c);
              io::stdout().flush().unwrap();
              lock.shell.input += &c.to_string();
            }
          },
        }
      },
      KeyCode::Backspace => {
        if lock.shell.cursor > 0 {
          let mut left = lock.shell.input[..(lock.shell.cursor-1) as usize].to_string();
          left += &lock.shell.input[lock.shell.cursor as usize..].to_string();
          lock.shell.input = left;
          lock.shell.cursor -= 1;

          //io::stdout().flush().unwrap();
          lock.shell.redraw();
        }
      },
      KeyCode::Arrow(d) => {
        match d {
          Direction::Up => todo!(),
          Direction::Down => todo!(),
          Direction::Left => {if lock.shell.cursor>0 {lock.shell.cursor-=1; print!("\x1b[1D"); io::stdout().flush().unwrap();}},
          Direction::Right => {if lock.shell.cursor <(lock.shell.input.len() as u16) {lock.shell.cursor+=1; print!("\x1b[1C"); io::stdout().flush().unwrap();}},
        }
      },
      _ => {}
    }

    /*ENTER
    */
  }
}

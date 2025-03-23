mod shell;
mod jobs;

use {
  shell::{builtins::ShellBuiltIns, language::SHLanguage},
  libc, 
  std::{env, ffi::CString, fmt::Display, fs::{self, read_to_string}, io::{self, Read, Write}, path::{Path, PathBuf}, process::Command, ptr::null, str, sync::Mutex}
};



enum Fructa {
  Exit,
  Pass,
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

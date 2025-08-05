mod shell;
mod jobs;

use {
  shell::{builtins::ShellBuiltIns, language::SHLanguage},
  libc, 
  std::{time::Duration, thread, env, ffi::CString, fmt::Display, fs::{self, read_to_string}, io::{self, Read, Write}, path::{Path, PathBuf}, process::Command, ptr::null, str, sync::{Arc,Mutex}}
};


#[derive(PartialEq)]
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
impl KeyEvent {
  fn from(str: &str) -> Self {
    match str {
      "\t"       => Self::from_keycode(KeyCode::Tab),
      "\n"       => Self::from_keycode(KeyCode::Enter),
      "\u{1b}"   => Self::from_keycode(KeyCode::Escape),
      "\u{7f}"   => Self::from_keycode(KeyCode::Backspace),
      "\u{1b}[3~"  => Self::from_keycode(KeyCode::Delete),
      x if x.starts_with("\u{1b}[") => {
        let mut modifiers = vec![];
        let mut rest = &x.chars().collect::<Vec<char>>()[2..];
        if rest.len() == 4 {
          if rest[0] == '1' && rest[1] == ';' {
            modifiers.push(match rest[2] {
              '2' => Modifier::Shift,
              '3' => Modifier::Alt,
              '5' => Modifier::Control,
              _ => panic!("unknown modifier: {rest:#?}")
            });
            rest = &rest[3..];
          }
        }
        let mut code = KeyCode::Arrow(match rest[0] {
          'A' => Direction::Up,
          'B' => Direction::Down,
          'C' => Direction::Right,
          'D' => Direction::Left,
          _ => panic!("unknown key: {rest:#?}")
        });
        KeyEvent {code, modifiers}
      },
      x if x.starts_with("\u{1b}") => KeyEvent {code: KeyCode::Char(x[1..].to_string()), modifiers: vec![Modifier::Alt]},
      /*"\u{1b}[A" => Self::from_keycode(KeyCode::Arrow(Direction::Up)),
      "\u{1b}[B" => Self::from_keycode(KeyCode::Arrow(Direction::Down)),
      "\u{1b}[C" => Self::from_keycode(KeyCode::Arrow(Direction::Right)),
      "\u{1b}[D" => Self::from_keycode(KeyCode::Arrow(Direction::Left)),
      */
      _ => {
        if str.len()==1 {
          return match str.chars().collect::<Vec<char>>()[0] {
            ' ' => Self::from_keycode(KeyCode::Char(" ".to_string())),
            '\u{1}' ..= '\u{20}' => KeyEvent {
              code: KeyCode::Char(
                {let x = str.escape_unicode().to_string(); str::from_utf8(&vec![u8::from_str_radix(&x[3..x.len()-1], 16).unwrap()+96]).unwrap().to_string()}
              ), 
              modifiers: vec![Modifier::Control]},
            _ => Self::from_keycode(KeyCode::Char(str.to_string())),
          };
        }
        Self::from_keycode(KeyCode::Char(str.to_string()))
      }
    }
  }
  fn from_keycode(code: KeyCode) -> Self {
    Self {code, modifiers: vec![]}
  }
}
#[derive(Debug,PartialEq)]
pub enum Modifier {
  Control,
  Shift,
  Alt,
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
  Tab,
  Backspace,
  Delete,
  Arrow(Direction),
  Char(String),
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




static mut WRITE_END: Option<i32> = None;

extern "C" fn sigint_handler(_sig: libc::c_int) {
  unsafe {
    if let Some(fd) = WRITE_END {
      let _ = libc::write(fd, b"SIGINT\n".as_ptr() as *const _, 7);
    }
  }
}


fn get_sighandler() -> libc::sighandler_t {
    sigint_handler as extern fn(libc::c_int) as *mut libc::c_void as libc::sighandler_t
}

fn read_key(output: Arc<Mutex<Channel>>) {
  //println!("startin");
  for i in io::stdin().bytes() {
    let mut lock = output.try_lock().unwrap();
    lock.message.push(i.unwrap());
    if lock.kill {break}
    drop(lock);
  }
}

#[derive(Debug)]
struct Channel {
  kill: bool,
  message: Vec<u8>,
}


const ms: u32 = 1000000;


fn handle_input(chr: &str) -> (Vec<u8>, Fructa) {
  //println!("{chr:#?}");
  match chr {
    "\u{1b}" => {
      let mut output = Arc::new(Mutex::new(
        Channel{kill: false, message: vec![]}));
      let clone = Arc::clone(&output);
      let a = thread::spawn(|| read_key(clone));
      thread::sleep(Duration::new(0, 50*ms));
      //println!("{output:#?}");
      let mut lock = output.lock().unwrap();
      let received = lock.message.clone();
      lock.kill = true;
      drop(lock);
      if evaluate_event(KeyEvent::from(&("\u{1b}".to_string() + str::from_utf8(&received).unwrap()))) == Fructa::Exit {
        return (vec![], Fructa::Exit);
      }
      let _ = a.join();
      let mut lock = output.lock().unwrap();
      let slopb = lock.message.clone();
      let slop = slopb[received.len()..].to_vec();
      //println!("slop: {slop:#?}");
      return (slop, Fructa::Pass)
    },
    _ => {(vec![], evaluate_event(KeyEvent::from(chr)))}//Char(String)
  }
}


fn evaluate_event(event: KeyEvent) -> Fructa {
  let mut lock = SIGHANDLER.lock().unwrap();
  match event.code {
    KeyCode::Enter => {
      lock.shell.input += "\n";
      //println!();
      match lock.shell.run() {
        Fructa::Exit => return Fructa::Exit,
        _ => {lock.shell.redraw()},
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
            match (&c) as &str { /* replace \u with just the key eg. `r` */
              "r" => {lock.shell.state = State::RSearch;lock.shell.redraw();},
              "l" => {lock.shell.clear();lock.shell.redraw();},
              "d" => {if lock.shell.input.is_empty() {println!();return Fructa::Exit} else {return Fructa::Pass}},
              _ => {},
            }
          } else {
            print!("{c}");
            let mut left = lock.shell.input[..lock.shell.cursor as usize].to_string();
            let right = &lock.shell.input[lock.shell.cursor as usize..];
            left += &c.to_string();
            left += right;
            lock.shell.input = left;
            lock.shell.cursor += 1;
            lock.shell.redraw_from_cursor();
            //print!("\x1b[1C");
            
            //print!("{}", c);
            //io::stdout().flush().unwrap();
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
        print!("\x1b[1D");
        lock.shell.redraw_from_cursor();
        //io::stdout().flush().unwrap();
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
  drop(lock);
  Fructa::Pass
}

use std::os::fd::AsRawFd;
use std::os::unix::net::UnixStream;
fn main() {
  setup_termios();
  enable_raw_mode();
  println!();
  io::stdout().flush().unwrap();
  


  unsafe{env::set_var("HOSTNAME", str::from_utf8(&Command::new("hostname").output().unwrap().stdout).unwrap().trim());} // this is cheating I think


  let (mut sigstream1, mut sigstream2) = UnixStream::pair().unwrap();

  unsafe {
    WRITE_END = Some(sigstream2.as_raw_fd());
    libc::signal(libc::SIGINT, get_sighandler());
  }


  thread::spawn(move || {
    let mut buf = [0u8; 128];
    loop {
      match sigstream1.read(&mut buf) {
        Ok(n) if n > 0 => {
          //let msg = String::from_utf8_lossy(&buf[..n]);
          let mut lock = SIGHANDLER.lock().unwrap();
          lock.shell.ctrl_c();
          drop(lock);
          //println!("Received: {}", msg);
        }
        Ok(_) => {} // no data
        Err(e) => {
          eprintln!("Read error: {}", e);
          break;
        }
      }
    }
  });




  let NULL: *const i8 = null();
  let mut lock = SIGHANDLER.lock().unwrap();
  lock.shell = Shell {input: String::new(), state: State::Normal, cursor: 0};
  lock.shell.redraw();
  drop(lock);

  let mut buf = vec![];
  let mut code = Fructa::Pass;
  'a: for c in io::stdin().bytes() {
    buf.push(c.unwrap());
    while !buf.is_empty() && let Ok(chr) = str::from_utf8(&buf) {
      (buf, code) = handle_input(chr);
      if code == Fructa::Exit {
        break 'a
      }
    }
  }
}

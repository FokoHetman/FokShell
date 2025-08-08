use {
  crate::shell::language::SHLanguage,
  crate::{Fructa,State},
  std::{ffi::CString, io::{self,Write}, process::Command},
};

pub trait ShellBuiltIns {
  fn cd(&mut self, path: String) -> i32;
  fn run(&mut self) -> Fructa;
  fn clear(&self);
  fn clear_line(&self);
  fn redraw(&self);
  fn redraw_from_cursor(&self);
  fn ctrl_c(&mut self);
  fn update‍_dir(&mut self);
}

impl ShellBuiltIns for crate::Shell {
  fn ctrl_c(&mut self) {
    println!("^C");
    self.input = String::new();
    self.cursor = 0;
    self.flags.set("last".to_string(), crate::Value::KeyEvent(
            crate::KeyEvent {code: crate::KeyCode::Char("c".to_string()), 
                modifiers: vec![crate::Modifier::Control]})).unwrap();
    self.redraw();
  }
  fn clear(&self) {
    println!("\x1b[2J\x1b[H");
    io::stdout().flush().unwrap();
  }
  fn clear_line(&self) {
    print!("\x1b[2K"); // clear entire line
    io::stdout().flush().unwrap();
  }
  fn redraw_from_cursor(&self) {
    /*if self.cursor > 0 {
      print!("\x1b[1D");
    }*/
    print!("\x1b[0K"); // clear from cursor
    if (self.cursor as usize ) < self.input.len() {
      print!("{}", self.input[self.cursor as usize..].to_string());
      print!("\x1b[{}D", self.input[self.cursor as usize..].to_string().len());
    }
    io::stdout().flush().unwrap();
  }
  fn redraw(&self) {
    self.clear_line();
    let mut combined_string = String::new();
    //combined_string += &format!("\x1b[1E\x1b[1F");
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
    println!();
    /*if self.input.is_empty() {
      return Fructa::Exit;
    }*/

    let tokens = self.tokenize(self.input.clone());
    //println!("TOKENS: {:#?}", tokens);
    let nodes = self.parse(tokens);
    //println!("NODES: {:#?}", nodes);
    self.evaluate_program(nodes);
    //println!();
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
    let out = unsafe {libc::chdir(path.as_ptr()) };
    self.update‍_dir();
    out
  }
  fn update‍_dir(&mut self) {
    self.dir = str::from_utf8(&Command::new("pwd").output().unwrap().stdout).unwrap().trim().to_string();
  }
}

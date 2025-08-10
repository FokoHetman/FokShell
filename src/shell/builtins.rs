use {
  crate::{shell::language::SHLanguage, Fructa, State},
  std::{env, ffi::CString, fs, io::{self,Write}, process::Command},
};

pub trait ShellBuiltIns {
  fn cd(&mut self, path: String) -> i32;
  fn run(&mut self) -> Fructa;
  fn exit(&mut self);
  fn clear(&self);
  fn clear_line(&self);
  fn redraw(&self);
  fn redraw_from_cursor(&self);
  fn ctrl_c(&mut self);
  fn update‍_dir(&mut self);
  fn chr_input(&self) -> Vec<String>;
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

  fn chr_input(&self) -> Vec<String> {
    displayable(self.input.clone())
  }
  
  fn redraw_from_cursor(&self) {
    /*if self.cursor > 0 {
      print!("\x1b[1D");
    }*/
    print!("\x1b[0K"); // clear from cursor
    
    let chr_input = self.chr_input();

    if (self.cursor as usize ) < chr_input.len() {
      print!("{}", chr_input[self.cursor as usize..].concat());
      print!("\x1b[{}D", chr_input[self.cursor as usize..].concat().len());
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

    let chr_input = self.chr_input();

    if chr_input.len()>0 { combined_string += &format!("\x1b[{}D", chr_input.len()); };
    if self.cursor>0 { combined_string += &format!("\x1b[{}C", self.cursor); }


    print!("{}", combined_string);
    io::stdout().flush().unwrap();
  }
  fn exit(&mut self) {
    let hpat = &(env::home_dir().unwrap()
    .as_os_str().to_str().unwrap().to_string() + "/.foksh_history");
    fs::write(hpat, self.history.join("\n")).unwrap();
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


pub fn displayable(str: String) -> Vec<String> {
  let mut res = vec![];
  
  let mut chars = str.bytes().collect::<Vec<u8>>();
  while chars.len() > 0 {
    let mut buf = vec![chars[0]];
    chars.remove(0);
    while let Err(_) = str::from_utf8(&buf) {
      buf.push(chars[0]);
      chars.remove(0);
    }
    res.push(str::from_utf8(&buf).unwrap().to_string());
  }
  res
}

/*fn nchars(str: &str, n: usize) -> String {
  let mut res = String::new();
  let mut str = str.bytes().collect::<Vec<u8>>();
  for i in 0..n {
    let mut buf = vec![str[0]];
    str.remove(0);
    while let Err(_) = str::from_utf8(&buf) {
      buf.push(str[0]);
      str.remove(0);
    }
    res += str::from_utf8(&buf).unwrap();
  }
  res
}*/

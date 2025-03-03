use {
  libc::{self, execvp}, 
  std::{env, ffi::CString, fmt::Display, io::{self,Write}, path::{Path, PathBuf}, ptr::null, str, process::Command}
};

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


fn tokenize(input: &String) -> Vec<&str> {
  return input[..input.len()-1].split(" ").collect::<Vec<&str>>();
}


trait ShellBuiltIns {
  fn cd(&mut self, path: String) -> i32;
}

struct Shell {
  path: PathBuf,
}
impl ShellBuiltIns for Shell {
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
    write!(f, "[{user}@{hostname}:{dir}]$ ")
  }
}

fn main() {
  env::set_var("HOSTNAME", str::from_utf8(&Command::new("hostname").output().unwrap().stdout).unwrap().trim()); // this is cheating I think
  let mut shell = Shell {
    path: PathBuf::from(&env::var("PWD").unwrap())
  };
  

  unsafe {
    libc::signal(libc::SIGINT, libc::SIG_IGN);
    /*libc::signal(libc::SIG_*/
  }

  let NULL: *const i8 = null();
  loop {
    print!("{}", shell);
    let _ = io::stdout().flush();
    let mut input = String::new();
    let _ = io::stdin().read_line(&mut input).unwrap();

    if input.is_empty() {
      break
    }

    let tokens = tokenize(&input);
    match tokens[0] {
      "cd" => {println!("{:#?}", shell.cd(tokens[1].to_string()))},
      _ => spawn_proc(tokens),
    }
  }
}

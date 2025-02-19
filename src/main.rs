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


struct ShellDisplay {
  dir: PathBuf,
}
impl Display for ShellDisplay {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let hostname = match &env::var("HOSTNAME") {
      Ok(s) => s.clone(),
      Err(_) => String::from("???"),
    };
    let user = match &env::var("USER") {
      Ok(s) => s.clone(),
      Err(_) => String::from("???"),
    };
    let dir = match &env::var("PWD") {
      Ok(s) => s.replace(&env::var("HOME").unwrap(), "~"),
      Err(_) => String::from("???"),
    };

    write!(f, "[{user}@{hostname}:{dir}]$ ")
  }
}


fn main() {
  env::set_var("HOSTNAME", str::from_utf8(&Command::new("hostname").output().unwrap().stdout).unwrap().trim()); // this is cheating I think
  let shell = ShellDisplay {
    dir: PathBuf::from(&env::var("PWD").unwrap())
  };
  

  loop {
    print!("{}", shell);
    let _ = io::stdout().flush();
    let mut input = String::new();
    let _ = io::stdin().read_line(&mut input).unwrap();
    spawn_proc(tokenize(&input));
  }
}

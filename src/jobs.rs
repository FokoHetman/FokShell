use {libc::execvp,std::{str,process::Command, ffi::CString, ptr::null}};

#[derive(Debug)]
pub enum JobStatus {
  Running,
  Suspended,
  Completed(i32),
}

pub struct Job {
  pub pid: libc::id_t,
  pub status: JobStatus,
}

// todo 
pub struct JobManager {
  jobs: Vec<Job>
}

pub fn spawn_proc_output(args: Vec<&str>) -> String {
  str::from_utf8(&Command::new(args[0]).args(args[1..].iter()).output().unwrap().stdout).unwrap().to_string()
}

pub fn spawn_proc(args: Vec<&str>) {
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

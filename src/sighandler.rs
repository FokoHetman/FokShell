use crate::ShellBuiltIns;

extern fn sigint_handler(signo: libc::c_int) {
  let mut lock = crate::SIGHANDLER.lock().unwrap();
  lock.shell.ctrl_c();
  drop(lock);
}

pub fn get_sighandler() -> libc::sighandler_t {
    sigint_handler as extern fn(libc::c_int) as *mut libc::c_void as libc::sighandler_t
}

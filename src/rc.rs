// vim:ft=rust:ts=2:sts=2:sw=2:et:
use std::fs;
use foklang;

fn load(rc_path: &str) -> crate::Shell {
  let rc = fs::read_to_string(rc_path).unwrap();
  let rc = foklang::evaluate(rc);
  let mut shell = crate::Shell::new();
  shell.load(rc);
}

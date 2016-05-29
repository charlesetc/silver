/* apricot utils */

type position = {
  line: int,
  column: int,
};

exception Apricot_error of string position;

let print_apricot_error error => {
  switch error {
    | Apricot_error desc pos => {
      Printf.printf "line:%d, column:%d\n  %s\n" pos.line pos.column desc;
    }
  }
};

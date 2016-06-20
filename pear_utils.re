/* pear utils */


type position = {
  line: int,
  column: int,
};

let string_of_char = String.make 1;

let rec split_at index list => {
  if (index == 0) {
    ([], list)
  } else {
    let (first_list, second_list) = split_at (index - 1) (List.tl list);
    ([List.hd list, ...first_list], second_list)
  }
};

exception Pear_error of string position;

exception Pear_bug of string position;

let empty_pear_bug str => {
    Pear_bug str {line: -1, column: -1}
};

let print_pear_error error => {
  switch error {
    | Pear_error desc pos => {
      Printf.printf "line:%d, column:%d\n  %s\n" pos.line pos.column desc;
    }
    | Pear_bug desc pos => {
      Printf.printf "INTERNAL COMPILER ERROR\nline:%d, column:%d\n  %s\n" pos.line pos.column desc;
    }
    | _ => ();
  }
};

/* apricot utils */


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

exception Apricot_error of string position;

exception Apricot_bug of string position;

let print_apricot_error error => {
  switch error {
    | Apricot_error desc pos => {
      Printf.printf "line:%d, column:%d\n  %s\n" pos.line pos.column desc;
    }
    | Apricot_bug desc pos => {
      Printf.printf "INTERNAL COMPILER ERROR\nline:%d, column:%d\n  %s\n" pos.line pos.column desc;
    }
    | _ => ();
  }
};

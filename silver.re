/* silver */

open Silver_token;

open Test_silver;

open Silver_balance;

open Silver_utils;

open Silver_parse;

open Silver_type;

let main () => {
  let state = "map { b : a 2 }\n";
  let state = "{ b : b }\n";
  Printf.printf "start:\n\t%s\n" state;
  let state = Stream.of_string state;
  let state = Silver_token.token state;
  let state = Silver_balance.balance state;
  let state = Silver_parse.parse state;
  let state = Silver_type.convert_to_silver_tree state;
  print_string "silver:\n\t";
  let str = string_of_silver_tree state;
  print_string str
};

Printexc.record_backtrace true;

switch (main ()) {
| _ => ()
| exception e => Silver_utils.print_silver_error e
};

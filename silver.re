/* silver */

open Silver_token;
open Test_silver;
open Silver_balance;
open Silver_utils;
open Silver_parse;
open Silver_type;
open Silver_type_old;
/* open Silver_compile; */

let main => {

  /* let state = Stream.of_channel stdin; */

  let state = "
map { b 1 2 }
";

  /*
  Printf.printf "start:\n\t%s\n\n" state;
  */

  let state = Stream.of_string state;

  let state = Silver_token.token state;

  let state = Silver_balance.balance state;

  let state = Silver_parse.parse state;

  let state = Silver_type_old.infer state;

  /*
   * let state = Silver_compile.compile state;

  print_string "tree:\n\t";
  print_string (Silver_type_old.string_of_typed_tree state);
  print_string "\n";
   */
};

switch (main ()) {
  | _ => ();
  | exception e => Silver_utils.print_silver_error e;
}

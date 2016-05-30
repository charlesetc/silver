/* apricot */

open Apricot_token;
open Test_apricot;
open Apricot_balance;
open Apricot_utils;
open Apricot_parse;
/* open Apricot_compile; */

let main => {

  /* let state = Stream.of_channel stdin; */

  let state = "class Apple { ( 2 ); wow print.test 3; }";
  /* Printf.printf "start: \"%s\"\n" state; */

  let state = Stream.of_string state;

  let state = Apricot_token.token state;

  let state = Apricot_balance.balance state;

  let state = Apricot_parse.parse state;

  /* let state = Apricot_compile.compile state; */

  /* print_string (Apricot_parse.string_of_abstract_tree state); */
};

switch (main ()) {
  | _ => ();
  | exception e => Apricot_utils.print_apricot_error e;
}

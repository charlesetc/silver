/* apricot */

open Apricot_token;
open Test_apricot;
open Apricot_balance;
open Apricot_utils;
open Apricot_parse;
open Apricot_type;
/* open Apricot_compile; */

let main => {

  /* let state = Stream.of_channel stdin; */

  let state = "map { x z : x y { x : x } x }";
  /* Printf.printf "start: \"%s\"\n" state; */

  let state = Stream.of_string state;

  let state = Apricot_token.token state;

  let state = Apricot_balance.balance state;

  let state = Apricot_parse.parse state;

  let state = Apricot_type.infer state;

  /* let state = Apricot_compile.compile state; */

  print_string (Apricot_type.string_of_typed_tree state);
};

switch (main ()) {
  | _ => ();
  | exception e => Apricot_utils.print_apricot_error e;
}

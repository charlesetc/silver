/* apricot */

open Apricot_token;
open Test_apricot;
open Apricot_balance;
open Apricot_utils;
/* open Apricot_parser; */
/* open Apricot_compiler; */

/* let state = Stream.of_channel stdin; */

let main => {

  let state = Stream.of_string "( { ( ) } hi )";

  let state = Apricot_token.token state;

  let state = Apricot_balance.balance state;

  /* let state = Apricot_parser.parse state; */

  /* let state = Apricot_compiler.compile state; */

  /* actually evaluate the stream */
  Stream.iter (fun x => {
    let (token, _) = x;
    print_string (Apricot_token.string_of_token token);
    print_char '\n';
  }) state
};

switch (main ()) {
  | _ => ();
  | exception (Apricot_error desc pos) => { Apricot_utils.print_apricot_error (Apricot_error desc pos) };
}

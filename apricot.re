/* apricot */

open Apricot_token;
/* use Apricot_parser; */
/* use Apricot_compiler; */

let state = Stream.of_channel stdin;

let state = Apricot_token.token state;

/* let state = Apricot_parser.parse state; */

/* let state = Apricot_compiler.compile state; */

Stream.iter (fun x => {
  let (token, _) = x;
  print_string (Apricot_token.string_of_token token);
  print_char '\n';
}) state

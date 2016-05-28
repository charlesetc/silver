/* test apricot */

open Apricot_token;

let list_of_stream stream => {
  let acc = ref [];
  Stream.iter (fun (v) => {
    acc := [v, ...!acc];
  }) stream;
  List.rev !acc
};

exception Token_assertion of (list Apricot_token.token) (list Apricot_token.token);

let token_equal a b => {
  switch (assert (a == b)) {
    | _ => print_string "\027[32m.\x1B[0m";
    | exception (Assert_failure _) => raise (Token_assertion a b);
  }
};

let test_tokens => {

  let assert_tokens string tokens => {
    let state = Stream.of_string string;
    let state = Apricot_token.token state;

    /* Get rid of positions. */
    let state = Stream.from (fun _ => {
      switch (Stream.next state) {
        | (s, _) => Some s;
        | exception Stream.Failure => None;
      }
    });
    token_equal (list_of_stream state) tokens;
  };

  assert_tokens "(" [Apricot_token.Left_round];
  assert_tokens ")" [Apricot_token.Right_round];
  assert_tokens "{" [Apricot_token.Left_curly];
  assert_tokens "}" [Apricot_token.Right_curly];

  assert_tokens "{ }" [
    Apricot_token.Left_curly,
    Apricot_token.Space,
    Apricot_token.Right_curly,
  ];

  assert_tokens "hi there" [
    Apricot_token.Identifier "hi",
    Apricot_token.Space,
    Apricot_token.Identifier "there",
  ];

  assert_tokens "'one' \"two\";:" [
    Apricot_token.String_literal "one",
    Apricot_token.Space,
    Apricot_token.String_literal "two",
    Apricot_token.Newline,
    Apricot_token.Colon,
  ];
};

let run_tests_with_regex regex => {
  let regex = Str.regexp regex;

  let tests = [
    ("test tokens", test_tokens),
  ];

  let errors = ref [];

  List.iter (fun (name, test) => {
    let run_this_test = switch (Str.search_forward regex name 0) {
      | _ => true;
      | exception Not_found => false;
    };

    if run_this_test {
      Printf.printf "\n: %s\n" name;
      switch (test ()) {
        | _ => (); /* print_string "\027[32m.\x1B[0m"; */
        | exception e => {
          errors := [e, ...!errors];
          print_string ("\027[31mF\x1B[0m");
        };
      }
    }
  }) tests;

  print_string "\n\n";

  List.iter (fun e => {
    switch e {
      | Token_assertion ts1 ts2 => {
        let print_tokens str tokens => {
          Printf.printf "%s: " str;
          List.iter
            (fun t => Printf.printf "%s " (Apricot_token.string_of_token t))
            tokens;
          print_char '\n';
        };
        print_tokens "found" ts1;
        print_tokens "asserted" ts2;
      };
      | _ => ();
    };
    print_string ((Printexc.to_string e) ^ "\n");
  }) !errors;
};

let check_tests => {
  switch (Sys.getenv "apricot_test") {
    | "all" => run_tests_with_regex(".*");
    | regex => run_tests_with_regex(regex);
    | exception Not_found => ();
  };
};

check_tests ();

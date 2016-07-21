/* test silver */

open Silver_token;

open Silver_utils;

open Silver_balance;

open Silver_type_old;

let list_of_stream stream => {
  let acc = ref [];
  Stream.iter (fun v => acc := [v, ...!acc]) stream;
  List.rev !acc
};

exception Generic_assertion of string;

exception Token_assertion of (list Silver_token.token) (list Silver_token.token);

exception String_assertion of string string;

let print_success () => print_string "\027[32m.\027[0m";

let token_equal a b =>
  switch (assert (a == b)) {
  | _ => print_success ()
  | exception Assert_failure _ => raise (Token_assertion a b)
  };

let test_tokens () => {
  let assert_tokens string tokens => {
    let state = Stream.of_string string;
    let state = Silver_token.token state;
    /* Get rid of positions. */
    let state = Stream.from (
      fun _ =>
        switch (Stream.next state) {
        | (s, _) => Some s
        | exception Stream.Failure => None
        }
    );
    token_equal (list_of_stream state) tokens
  };
  assert_tokens "(" [Silver_token.Left_round];
  assert_tokens ")" [Silver_token.Right_round];
  assert_tokens "{" [Silver_token.Left_curly];
  assert_tokens "}" [Silver_token.Right_curly];
  assert_tokens "{ }" [Silver_token.Left_curly, Silver_token.Space, Silver_token.Right_curly];
  assert_tokens
    "hi there" [Silver_token.Identifier "hi", Silver_token.Space, Silver_token.Identifier "there"];
  assert_tokens "hi." [Silver_token.Identifier "hi", Silver_token.Dot];
  assert_tokens "hi.;" [Silver_token.Identifier "hi", Silver_token.Dot, Silver_token.Newline];
  assert_tokens "hi.there" [Silver_token.Identifier "hi", Silver_token.Dot_literal "there"];
  assert_tokens
    "'one' \"two\";:"
    [
      Silver_token.String_literal "one",
      Silver_token.Space,
      Silver_token.String_literal "two",
      Silver_token.Newline,
      Silver_token.Colon
    ]
};

let test_balanced () => {
  let assert_balanced string => {
    let state = Stream.of_string string;
    let state = Silver_token.token state;
    let state = Silver_balance.balance state;
    /* Strictly evaluate the stream */
    let state = Stream.iter (fun s => ()) state;
    print_success ()
  };
  let assert_not_balanced string => {
    let state = Stream.of_string string;
    let state = Silver_token.token state;
    let state = Silver_balance.balance state;
    /* Strictly evaluate the stream */
    switch (Stream.iter (fun s => ()) state) {
    | _ => raise (Generic_assertion "tokens are balanced")
    | exception Silver_error _ _ => print_success ()
    }
  };
  assert_balanced "{}";
  assert_balanced "{ }";
  assert_balanced "{ ()}";
  assert_balanced "({})";
  assert_balanced "(){}";
  assert_not_balanced "{ ";
  assert_not_balanced " }";
  assert_not_balanced "} ";
  assert_not_balanced "(})";
  assert_not_balanced "()}"
};

let remove_from_string c s => {
  let output = ref "";
  let add_char c => output := !output ^ Silver_utils.string_of_char c;
  let s = Stream.of_string s;
  Stream.iter
    (
      fun char =>
        switch char {
        | new_char when new_char == c => ()
        | a => add_char a
        }
    )
    s;
  !output
};

let test_basic_parsing () => {
  let assert_parsed string expected => {
    let state = Stream.of_string string;
    let state = Silver_token.token state;
    let state = Silver_balance.balance state;
    let state = Silver_parse.parse state;
    let actual = Silver_parse.string_of_abstract_tree state;
    let expected = remove_from_string ' ' expected;
    let actual = remove_from_string ' ' actual;
    switch (assert (actual == expected)) {
    | _ => print_success ()
    | exception Assert_failure _ => raise (String_assertion actual expected)
    }
  };
  assert_parsed "" "{}";
  assert_parsed "hi\nthere" "{:hi;:there;}";
  assert_parsed "((this\nthat))" "{(:this :that);}";
  assert_parsed "((((hi))))" "{:hi;}";
  assert_parsed
    "(square { print 'hi' ; print one })" "{(:square {(:print \"hi\"); (:print :one);});}";
  assert_parsed "{ hi: \n there }" "{{lambda :hi of :there;};}";
  assert_parsed "{ \n\n\n hi: \n\n\n there }" "{{lambda :hi of :there;};}";
  assert_parsed "{ hi: there }" "{{lambda :hi of :there;};}";
  assert_parsed
    "{ hi: there } { hi: there }" "{({lambda :hi of :there;} {lambda :hi of :there;});}";
  assert_parsed "{ hi: there }\n{ hi: there }" "{{lambda :hi of :there;};{lambda :hi of :there;};}";
  assert_parsed "<color:blue>" "{(:struct:color:blue);}"
};

let test_type_inference () => {
  let assert_typed string expected => {
    let state = Stream.of_string string;
    let state = Silver_token.token state;
    let state = Silver_balance.balance state;
    let state = Silver_parse.parse state;
    let (silver_tree, constraints) = Silver_type.convert_to_silver_tree state;
    Silver_type.reset_count ();
    let actual = Silver_type.string_of_silver_type (Silver_type.type_of_silver_tree silver_tree);
    let actual = remove_from_string ' ' actual;
    let expected = remove_from_string ' ' expected;
    switch (assert (actual == expected)) {
    | _ => print_success ()
    | exception Assert_failure _ =>
      print_string (Silver_type.string_of_constraints constraints);
      print_string (Silver_type.string_of_silver_tree silver_tree);
      raise (String_assertion actual expected)
    }
  };
  assert_typed "x" "(unit -> 'a)";
  assert_typed "{x : x}" "(unit -> ('a -> 'a))";
  assert_typed "{x : x a}" "(unit -> (('a -> 'c) -> 'c))";
  assert_typed "{x a: a 2; x a}" "(unit -> (((int -> 'c)-> 'f) -> ((int -> 'c) -> 'f)))";
  assert_typed "{x: x 1}" "(unit-> ((int->'c) -> 'c))";
  assert_typed "{x: x 1 2 3}" "(unit -> ((int-> (int-> (int -> 'g)))-> 'g))"
};

let run_tests_with_regex regex => {
  let regex = Str.regexp regex;
  let tests = [
    ("test tokens", test_tokens),
    ("test balanced", test_balanced),
    ("test basic parsing", test_basic_parsing),
    ("test type inference", test_type_inference)
  ];
  let errors = ref [];
  List.iter
    (
      fun (name, test) => {
        let run_this_test =
          switch (Str.search_forward regex name 0) {
          | _ => true
          | exception Not_found => false
          };
        if run_this_test {
          Printf.printf "\n: %s\n" name;
          switch (test ()) {
          | _ => () /* print_string "\027[32m.\x1B[0m"; */
          | exception e =>
            errors := [e, ...!errors];
            print_string "\027[31mF\027[0m"
          }
        }
      }
    )
    tests;
  print_string "\n\n";
  List.iter
    (
      fun e => {
        switch e {
        | String_assertion s1 s2 => Printf.printf "found:    %s\nasserted: %s\n" s1 s2
        | Token_assertion ts1 ts2 =>
          let print_tokens str tokens => {
            Printf.printf "%s: " str;
            List.iter (fun t => Printf.printf "%s " (Silver_token.string_of_token t)) tokens;
            print_char '\n'
          };
          print_tokens "found" ts1;
          print_tokens "asserted" ts2
        | _ => Silver_utils.print_silver_error e
        };
        print_string (Printexc.to_string e ^ "\n")
      }
    )
    !errors
};

let check_tests () =>
  switch (Sys.getenv "silver_test") {
  | "all" => run_tests_with_regex ".*"
  | regex => run_tests_with_regex regex
  | exception Not_found => ()
  };

check_tests ();

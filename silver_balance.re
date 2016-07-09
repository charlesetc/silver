/* silver balance */

open Silver_utils;

open Silver_token;

let balance stream => {
  let stack = Stack.create ();
  let matched_symbols = [Silver_token.Left_round, Silver_token.Left_curly];
  let is_matched_symbol symbol =>
    switch (List.find (fun x => x == symbol) matched_symbols) {
    | exception Not_found => false
    | _ => true
    };
  let ensure starting_token left_token right_position =>
    switch (Stack.pop stack) {
    | (right_token, left_position) =>
      if (right_token == left_token) {
        ()
      } else {
        let error_string =
          Printf.sprintf
            "token %s at line %d, column %d, does not match token %s"
            (string_of_token starting_token)
            right_position.line
            right_position.column
            (string_of_token right_token);
        raise (Silver_error error_string left_position)
      }
    | exception Stack.Empty =>
      let error_string =
        Printf.sprintf
          "token %s found without previous matching token" (string_of_token starting_token);
      raise (Silver_error error_string right_position)
    };
  let inner_balance _ => {
    let transparent_return_value = Stream.peek stream;
    switch (Stream.next stream) {
    | (Silver_token.Right_round, right_position) =>
      ensure Silver_token.Right_round Silver_token.Left_round right_position
    | (Silver_token.Right_curly, right_position) =>
      ensure Silver_token.Right_curly Silver_token.Left_curly right_position
    | (t, left_position) when is_matched_symbol t => Stack.push (t, left_position) stack
    | _ => ()
    | exception Stream.Failure =>
      switch (Stack.pop stack) {
      | (t, left_position) =>
        let error_string =
          Printf.sprintf "token %s found without matching close token" (string_of_token t);
        raise (Silver_error error_string left_position)
      | exception Stack.Empty => ()
      }
    };
    transparent_return_value
  };
  Stream.from inner_balance
};

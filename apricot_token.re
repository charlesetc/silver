/* apricot token */

type token = Identifier of string
  | String_literal of string
  | Left_curly
  | Right_curly
  | Left_round
  | Right_round
  | Newline
  | Colon
  | Space
  ;

let string_of_token tokens => switch tokens {
  | Left_curly => "left curly";
  | Right_curly => "right curly";
  | Left_round => "left round";
  | Right_round => "right round";
  | Space => "space";
  | Colon => "colon";
  | Identifier s => ":" ^ s;
  | String_literal s => "\"" ^ s ^ "\""
  | Newline => "newline"
};

let is_split_char c => {
  let split_chars = ";\"\n'{}(). ";
  switch (String.index split_chars c) {
    | exception Not_found => false;
    | _ => true;
  }
};

type position = {
  line: int,
  column: int,
};

exception Token_error of string position;

let next_character_generator position stream () => {
  let char = Stream.next stream;
  if (char == '\n') {
    position := {
      line: (!position).line + 1,
      column: 0,
    };
  } else {
    position := {
      line: (!position).line,
      column: (!position).column + 1,
    };
  };
  ()
};

let string_of_char = String.make 1;

let token stream => {

  let position = ref {
    line: 0,
    column: 0,
  };

  let advance_character = next_character_generator position stream;

  let token_error desc => raise (Token_error desc !position);

  let rec read_string quote => {
    switch (Stream.peek stream) {
      | Some c when c == quote => { "" } ;
      | Some c => { advance_character () ; string_of_char c ^ read_string quote };
      | None => token_error "hit end of file when parsing string";
    }
  };

  let rec read_identifier => {
    switch (Stream.peek stream) {
      | Some c when is_split_char c => "" ;
      | Some c => { advance_character () ; string_of_char c ^ read_identifier () };
      | None => "";
    }
  };

  let gen_token tok => Some { advance_character () ; tok };

  let stream_fn _ => {
    let character = switch (Stream.peek stream) {
      | Some '}' => gen_token Right_curly;
      | Some '{' => gen_token Left_curly;
      | Some ':' => gen_token Colon;
      | Some ')' => gen_token Right_round;
      | Some '(' => gen_token Left_round;
      | Some '\n' => gen_token Newline;
      | Some ';' => gen_token Newline;
      | Some ' ' => gen_token Space;
      | Some '"' => {
        advance_character ();
        let char = String_literal (read_string '"');
        advance_character ();
        Some char
      }
      | Some '\'' => {
        advance_character ();
        let char = String_literal (read_string '\'');
        advance_character ();
        Some char
      }
      | Some _ => Some (Identifier (read_identifier ()));
      | None => None;
    };

    switch character {
      | Some c => Some (c, !position);
      | None => None;
    }
  };

  Stream.from stream_fn
}

/* apricot parser */

open Utils;

type location = {
  line: int,
  column: int,
};

type token = {
  value: bytes,
  location: location,
};

let parse_tokens string => {

  /* a mapping from functions to
     appropriate tokens */

  let location = {line: 0, column: 0};
  
  let gen_token value : token => {location, value};

  let parse_curly s => String.get s 0 == '}';

  let token_mapping = [
    (parse_curly, gen_token "}")
  ];

}

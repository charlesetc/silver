/* silver type */

open Silver_utils;

open Silver_parse;

/* These three types make up all possible types in silver */
type attribute = (string, silver_type)
and small_type =
  | Unit | Integer | Float | Function of silver_type silver_type | Object of (list attribute)
and silver_type = | Generic of int | Abstract of small_type | Concrete of small_type;

type generic_silver_tree 'a =
  | Symbol of silver_type 'a
  | Function_def of silver_type (generic_silver_tree 'a) (list (generic_silver_tree 'a))
  | Function_app of silver_type (generic_silver_tree 'a) (generic_silver_tree 'a);

/* This is a concrete tree without type parameters */
type silver_tree = generic_silver_tree (string, Silver_utils.position);

let convert_to_silver_tree abstract_tree => {
  let starting_table = Hashtbl.create 16; /* 16 is an arbirtrary number */
  let symbol_tracker = ref 0;
  let generic_type () => {
    let a = !symbol_tracker;
    symbol_tracker := a + 1;
    Generic a
  };
  let type_for_symbol s => generic_type ();
  let rec convert tree symbol_table =>
    switch tree {
    | Silver_parse.Symbol s => type_for_symbol s
    | _ => raise Exit
    };
  convert abstract_tree starting_table
};

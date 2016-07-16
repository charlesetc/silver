/* silver type */

open Silver_utils;

open Silver_token;

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
type silver_tree = generic_silver_tree (Silver_token.token, Silver_utils.position);

let rec string_of_silver_tree tree =>
  switch tree {
  | Symbol _ (a, _) => Silver_token.string_of_token a
  | Function_def _ argument body =>
    "{ " ^
      string_of_silver_tree argument ^
      " : " ^
      String.concat "; " (List.map string_of_silver_tree body) ^
      "}"
  | Function_app _ argument ret =>
    "(" ^ string_of_silver_tree argument ^ " " ^ string_of_silver_tree ret ^ ")"
  };

/* generate a different generic type each time */
let symbol_tracker = ref 0;

let generic_type () => {
  let a = !symbol_tracker;
  symbol_tracker := a + 1;
  Generic a
};

/* this function converts an abstract tree to a silver tree (with types) */
let unit_token = Symbol (Concrete Unit) (Silver_token.Unit, {line: 0, column: 0});

let rec initial_to_silver abstract_tree =>
  switch abstract_tree {
  | Silver_parse.Symbol s => Symbol (generic_type ()) s
  | Silver_parse.Call_list lst =>
    switch (List.length lst) {
    | 0
    | 1 => raise (Silver_utils.empty_silver_bug "a call list cannot have 0 or 1 argument(s)!")
    | 2 =>
      Function_app
        (generic_type ()) (initial_to_silver (List.hd lst)) (initial_to_silver (List.nth lst 1))
    | _ =>
      Function_app
        (generic_type ())
        (initial_to_silver (List.hd lst))
        (initial_to_silver (Silver_parse.Call_list (List.tl lst)))
    }
  | Silver_parse.Lambda_list arguments body =>
    switch (List.length arguments) {
    | 0 => Function_def (generic_type ()) unit_token (List.map initial_to_silver body)
    | 1 =>
      Function_def
        (generic_type ()) (initial_to_silver (List.hd arguments)) (List.map initial_to_silver body)
    | _ =>
      Function_def
        (generic_type ())
        (initial_to_silver (List.hd arguments))
        [initial_to_silver (Silver_parse.Call_list (List.tl body))]
    }
  | Silver_parse.Sequence_list body => initial_to_silver (Silver_parse.Lambda_list [] body)
  };

let type_of_silver_tree tree =>
  switch tree {
  | Symbol silver_type _ => silver_type
  | Function_app silver_type argument action => silver_type
  | Function_def silver_type argument body => silver_type
  };

let rec map_over_tree fapp fdef fsymbol (tree: silver_tree) =>
  switch tree {
  | Function_app silver_type argument action =>
    let this_round =
      switch fapp {
      | None => []
      | Some f => f silver_type argument action
      };
    List.append
      (List.concat (List.map (map_over_tree fapp fdef fsymbol) [argument, action])) this_round
  | Function_def silver_type argument body =>
    let this_round =
      switch fdef {
      | None => []
      | Some f => f silver_type argument body
      };
    List.append (List.concat (List.map (map_over_tree fapp fdef fsymbol) body)) this_round
  | Symbol silver_tree a =>
    switch fsymbol {
    | None => []
    | Some f => f silver_tree a
    }
  };

/* This can be beautified in some way */
let constrain_function_bindings constraints silver_tree => {
  let rec apply_bindings_to_tree constraints table silver_tree =>
    switch silver_tree {
    | Symbol silver_type (Identifier ident, position) =>
      switch (Hashtbl.find table ident) {
      | a_type => [(a_type, silver_type), ...constraints]
      }
    | Function_app silver_type argument action =>
      List.concat [
        apply_bindings_to_tree constraints table argument,
        apply_bindings_to_tree constraints table action,
        constraints
      ]
    | Function_def silver_type argument body =>
      let table = Hashtbl.copy table;
      switch argument {
      | Symbol silver_type (Identifier ident, position) => Hashtbl.add table ident silver_type
      | _ => ()
      };
      List.append
        constraints (List.concat (List.map (apply_bindings_to_tree constraints table) body))
    | Symbol _ _ => constraints /* ignore anything that's not an identifier - it is handled elsewhere */
    };
  apply_bindings_to_tree constraints (Hashtbl.create 16) silver_tree /* 16 is arbitrary */
};

let constrain_function_calls =
  map_over_tree
    (
      Some (
        fun silver_type argument action => [
          (
            type_of_silver_tree action,
            Concrete (Function (type_of_silver_tree argument) silver_type)
          )
        ]
      )
    )
    None
    None;

let constrain_function_definitions =
  map_over_tree
    None
    (
      Some (
        fun silver_type argument body => [
          (
            silver_type,
            Concrete (
              Function
                (type_of_silver_tree argument) (type_of_silver_tree (Silver_utils.last_of body))
            )
          )
        ]
      )
    )
    None;

let convert_to_silver_tree abstract_tree => {
  let silver_tree = initial_to_silver abstract_tree;
  let constraints = [];
  let constraints = constrain_function_bindings constraints silver_tree;
  let constraints = List.append constraints (constrain_function_calls silver_tree);
  let constraints = List.append constraints (constrain_function_definitions silver_tree);
  silver_tree
};

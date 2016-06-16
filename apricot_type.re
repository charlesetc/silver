/* apricot type */

/*
 * This file's purpose is to take a generic syntax tree
 * that is designed to be maleable and represent as closely
 * as possible to the actual code, and move to a stricter
 * tree that can be easily type-checked and compiled.
 *
 */

open Apricot_utils;
open Apricot_parse;
open Apricot_token;

type apricot_type = Unit
                  | Integer
                  | Float
                  | String
                  | Function of apricot_type apricot_type
                  | Generic of int
                  ;

type typed_unit = {
    apricot_type: apricot_type,
    position: Apricot_utils.position,
    data: string,
};

type typed_tree = Symbol of typed_unit
                | Function_call of apricot_type (list typed_tree)
                | Function_definition of apricot_type typed_unit (list typed_tree)
                ;

let rec string_of_typed_tree tree => {
    let string_of_tu tu => switch tu.apricot_type {
        | Unit => "unit"
        | _ => tu.data
    };

    switch tree {
        | Symbol tu => string_of_tu tu
        | Function_call ty trees => {
            "(" ^ (String.concat " " (List.map string_of_typed_tree trees)) ^ ")"
        }
        | Function_definition ty tu trees => {
            "{" ^ (string_of_tu tu) ^  " -> " ^ (String.concat "; " (List.map string_of_typed_tree trees)) ^ "}"
        }
    }
};

let infer_literal_type str => {
    /* Make this better eventually */
    Integer
};

let rec convert_to_typed_tree tree => {

    let i = ref 0;
    let generic_type () => {
        i := !i + 1;
        Generic !i
    };

    switch tree {
        | Apricot_parse.Symbol (token, position) => {
            switch token {
                | Apricot_token.Identifier str => Symbol{
                        apricot_type: (infer_literal_type str),
                        position: position,
                        data: str,
                }
                | Apricot_token.String_literal str => Symbol {
                        apricot_type: String,
                        position: position,
                        data: str,
                }
                |  _ => raise (Apricot_bug (Printf.sprintf
                                            "cannot type-infer with token %s"
                                            (Apricot_token.string_of_token token)) position)
            }
        }
        | Apricot_parse.Call_list abstract_trees => Function_call
            (generic_type ())
            (List.map convert_to_typed_tree abstract_trees)
        | Apricot_parse.Lambda_list arguments abstract_trees => {
            switch (List.hd arguments) {
                | Symbol (token, position) => {
                    switch token {
                        | Identifier data => {
                            Function_definition
                                (Function (generic_type ()) (generic_type ()))
                                {
                                    apricot_type: (generic_type ()),
                                    position: position,
                                    data: data,
                                }
                                [
                                    convert_to_typed_tree (Apricot_parse.Lambda_list (List.tl arguments) abstract_trees)
                                ]
                        }
                        | _ => raise (Apricot_utils.Apricot_error
                                      (Printf.sprintf
                                           "found token %s, which is not an identifier, \
                                           as an argument to a function definition"
                                           (Apricot_token.string_of_token token))
                                      position)
                    }
                }
                | false_argument => raise (Apricot_utils.Apricot_error
                                            (Printf.sprintf
                                                "found tee %s, which is not an identifier, \
                                                as an argumnt to a function definition"
                                                (Apricot_parse.string_of_abstract_tree false_argument))
                                            {line: -1, column: -1})
                | exception (Failure "hd") => {
                    Function_definition
                        (Function Unit (generic_type ()))
                        {
                            apricot_type: Unit,
                            position: {line: -1, column: -1},
                            data: "", /* it's a unit */
                        }
                        (List.map convert_to_typed_tree abstract_trees)
                }
            }
        }
        | Apricot_parse.Sequence_list trees => {
            Function_call (generic_type ())  [convert_to_typed_tree (Apricot_parse.Lambda_list [] trees),
                        Symbol {
                            apricot_type: Unit,
                            position: {line: -1, column: -1},
                            data: "", /* it's a unit */
                        }
]
        }
    }
};

/* let infer_types tree => {
     let rec occurs (
}; */

let infer tree => {
    let tree = convert_to_typed_tree tree;
    /* let tree = infer_types tree; */
    tree
};

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

let string_type_of_int i => {
    let number = i / 26;
    let append_str = if (number == 0) {
        ""
    } else {
        string_of_int number
    };
    "'" ^ string_of_char (Char.chr (i + (Char.code 'a'))) ^ append_str
};

let rec string_of_apricot_type at => {
    switch at {
         | Unit => "unit"
         | Integer => "int"
         | Float => "float"
         | String => "string"
         | Function a b => "(" ^ string_of_apricot_type a ^ "=>" ^ string_of_apricot_type b ^ ")"
         | Generic i => string_type_of_int i
    }
};

type typed_unit = {
    apricot_type: apricot_type,
    position: Apricot_utils.position,
    data: string,
};

type typed_tree = Symbol of typed_unit
                | Function_call of apricot_type typed_tree typed_tree
                | Function_definition of apricot_type typed_unit (list typed_tree)
                ;

let rec string_of_typed_tree tree => {
    let string_of_tu tu => switch tu.apricot_type {
        | Unit => "unit"
        | _ => tu.data  ^ ":" ^ string_of_apricot_type tu.apricot_type
    };

    switch tree {
        | Symbol tu => string_of_tu tu
        | Function_call ty tree1 tree2 => {
            "(" ^ " " ^ string_of_typed_tree tree1 ^ " " ^ string_of_typed_tree tree2 ^ " " ^ ")"
        }
        | Function_definition ty tu trees => {
            "{" ^ (string_of_tu tu) ^  " -> " ^ (String.concat "; " (List.map string_of_typed_tree trees)) ^ "}"
        }
    }
};

let i = ref 0;

let rec convert_to_typed_tree table tree => {
    let generic_type () => {
        let a = !i;
        i := !i + 1;
        Generic a
    };

    let infer_literal_type table str => {
        let looks_like_integer str => {
            false
        };
        switch (Hashtbl.find table str) {
            | t => t;
            | exception Not_found => {
                if (looks_like_integer str) {
                    Integer
                } else {
                    generic_type ()
                }
            }
        }
    };

    switch tree {
        | Apricot_parse.Symbol (token, position) => {
            switch token {
                | Apricot_token.Identifier str => Symbol{
                        apricot_type: (infer_literal_type table str),
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
        | Apricot_parse.Call_list abstract_trees => {
            let rec handle_call_list abstract_trees => {
                switch (List.length abstract_trees) {
                    | 1 => {
                        Function_call
                            (generic_type ())
                            (convert_to_typed_tree table (List.hd abstract_trees))
                            (Symbol {
                                apricot_type: Unit,
                                position: {line: -1, column: -1},
                                data: "", /* it's a unit */
                            })
                    }
                    | _ => {
                        Function_call
                            (generic_type ())
                            (handle_call_list (List.tl abstract_trees))
                            (convert_to_typed_tree table (List.hd abstract_trees))
                    }
                }
            };
            handle_call_list (List.rev abstract_trees)
        }
        | Apricot_parse.Lambda_list arguments abstract_trees => {
            switch (List.hd arguments) {
                | Symbol (token, position) => {
                    switch token {
                        | Identifier data => {
                            let type_of_argument = generic_type ();
                            let table = Hashtbl.copy table;
                            Hashtbl.add table data type_of_argument;
                            Function_definition
                                (generic_type ())
                                {
                                    apricot_type: type_of_argument,
                                    position: position,
                                    data: data,
                                }
                                [
                                    convert_to_typed_tree table (Apricot_parse.Lambda_list (List.tl arguments) abstract_trees)
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
                        (List.map (convert_to_typed_tree table) abstract_trees)
                }
            }
        }
        | Apricot_parse.Sequence_list trees => {
            Function_call Unit
                (convert_to_typed_tree table (Apricot_parse.Lambda_list [] trees))
                (Symbol {
                    apricot_type: Unit,
                    position: {line: -1, column: -1},
                    data: "", /* it's a unit */
                })
        }
    }
};

let get_type tree => {
    switch tree {
        | Symbol t_unit => {
            t_unit.apricot_type
        }
        | Function_call a_type function_itself argument => {
            a_type
        }
        | Function_definition a_type t_unit trees => {
            a_type
        }
    }
};

let infer_types tree => {
   let collect_constraints tree existing : list 'a => {
        switch tree {
            | Symbol t_unit => {
                existing
            }
            | Function_call a_type function_itself argument => {
                [(get_type argument, Function (get_type function_itself) a_type), ...existing]
            }
            | Function_definition a_type t_unit trees => {
                [(a_type, Function t_unit.apricot_type (get_type (List.nth trees (List.length trees - 1)))), ...existing]
            }
        }
    };

    let constraints = collect_constraints tree [];

    constraints
};

let infer tree => {
    let tree = convert_to_typed_tree (Hashtbl.create 16) tree;
    /* let tree = infer_types tree; */
    tree
};

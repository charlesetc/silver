/* pear type old */

/*
 * This file's purpose is to take a generic syntax tree
 * that is designed to be maleable and represent as closely
 * as possible to the actual code, and move to a stricter
 * tree that can be easily type-checked and compiled.
 *
 */

open Pear_utils;
open Pear_parse;
open Pear_token;

type pear_type = Unit
                  | Integer
                  | Float
                  | String
                  | Function of pear_type pear_type
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

let rec string_of_pear_type at => {
    switch at {
         | Unit => "unit"
         | Integer => "int"
         | Float => "float"
         | String => "string"
         | Function a b => "(" ^ string_of_pear_type a ^ "=>" ^ string_of_pear_type b ^ ")"
         | Generic i => string_type_of_int i
    }
};

type typed_unit = {
    pear_type: pear_type,
    position: Pear_utils.position,
    data: string,
};

type typed_tree = Symbol of typed_unit
                | Function_call of pear_type typed_tree typed_tree
                | Function_definition of pear_type typed_unit (list typed_tree)
                ;

let rec string_of_typed_tree tree => {
    let string_of_tu tu => switch tu.pear_type {
        | Unit => "unit"
        /* | _ => tu.data  ^ ":" ^ string_of_pear_type tu.pear_type */
        | _ => tu.data  ^ ":" ^ string_of_pear_type tu.pear_type
    };

    switch tree {
        | Symbol tu => string_of_tu tu
        | Function_call ty tree1 tree2 => {
            "(" ^ " " ^ string_of_typed_tree tree1 ^ " $ " ^ string_of_typed_tree tree2 ^ " " ^ ") : " ^ string_of_pear_type ty
        }
        | Function_definition ty tu trees => {
            "{" ^ (string_of_tu tu) ^  " -> " ^ (String.concat "; " (List.map string_of_typed_tree trees)) ^ "} : " ^ string_of_pear_type ty
        }
    }
};

let generic_type_count = ref 0;
let reset_count => generic_type_count := 0;

let rec convert_to_typed_tree table tree => {
    /* This function takes a hash table (starts as empty),
     * and an abstract tree. It converts an abstract tree
     * to a typed tree.
     *
     * There doesn't need to be a correlation between the
     * typed tree and the original code, unlike the abstract
     * tree. (This is because the abstract tree will be used
     * for macros)
     */

    let generic_type () => {
        /* make a new generic type that hasn't been used before */
        let a = !generic_type_count;
        generic_type_count := !generic_type_count + 1;
        Generic a
    };

    let infer_literal_type table str => {
        /* Here we convert numbers/floats - things that
         * would normally be parsed as symbols to
         * their own type.
         */
        let looks_like_integer str : bool => switch (int_of_string str) {
            | _ => true
            | exception _ => false
        };
        switch (Hashtbl.find table str) {
            /* If there is already a type for this symbol use it */
            | t => t;
            | exception Not_found => {
                if (looks_like_integer str) {
                    Integer
                } else {
                    /* otherwise make a new one */
                    let typ = generic_type ();
                    Hashtbl.add table str typ;
                    typ
                }
            }
        }
    };

    switch tree {
        | Pear_parse.Symbol (token, position) => {

            /* Symbols have a single token - each of which
             * is associated with a type
             */
            switch token {
                | Pear_token.Identifier str => {
                    Symbol{
                        pear_type: (infer_literal_type table str),
                        position: position,
                        data: str,
                    }
                }
                | Pear_token.String_literal str => {
                    Symbol {
                        pear_type: String,
                        position: position,
                        data: str,
                    }
                }
                |  _ => raise (Pear_bug (Printf.sprintf
                                            "cannot type-infer with token %s"
                                            (Pear_token.string_of_token token)) position)
            }
        }
        | Pear_parse.Call_list abstract_trees => {

            /* this will turn `(2 3 4 5 6)` into
             * `(2 (3 (4 (5 6))))` */

            let rec handle_call_list abstract_trees => {
                switch (List.length abstract_trees) {
                    | 2 => {
                        Function_call
                            (generic_type ())
                            (convert_to_typed_tree table (List.nth abstract_trees 1))
                            (convert_to_typed_tree table (List.hd abstract_trees))
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
        | Pear_parse.Lambda_list arguments abstract_trees => {
            switch (List.hd arguments) {
                | Symbol (token, position) => {
                    switch token {
                        | Identifier data => {
                            let type_of_argument = generic_type ();
                            let table = Hashtbl.copy table;
                            Hashtbl.add table data type_of_argument;
                            if (List.length arguments == 1) {
                                Function_definition
                                    (generic_type ())
                                    {
                                        pear_type: type_of_argument,
                                        position: position,
                                        data: data,
                                    }
                                    (List.map (convert_to_typed_tree table) abstract_trees)
                            } else {
                                Function_definition
                                    (generic_type ())
                                    {
                                        pear_type: type_of_argument,
                                        position: position,
                                        data: data,
                                    }
                                    [
                                        convert_to_typed_tree table (Pear_parse.Lambda_list (List.tl arguments) abstract_trees)
                                    ]
                            }
                        }
                        | _ => raise (Pear_utils.Pear_error
                                      (Printf.sprintf
                                           "found token %s, which is not an identifier, \
                                           as an argument to a function definition"
                                           (Pear_token.string_of_token token))
                                      position)
                    }
                }
                | false_argument => raise (Pear_utils.Pear_error
                                            (Printf.sprintf
                                                "found tee %s, which is not an identifier, \
                                                as an argumnt to a function definition"
                                                (Pear_parse.string_of_abstract_tree false_argument))
                                            {line: -1, column: -1})
                | exception (Failure "hd") => {
                    Function_definition
                        (Function Unit (generic_type ()))
                        {
                            pear_type: Unit,
                            position: {line: -1, column: -1},
                            data: "", /* it's a unit */
                        }
                        (List.map (convert_to_typed_tree table) abstract_trees)
                }
            }
        }
        | Pear_parse.Sequence_list trees => {
            Function_call Unit
                (convert_to_typed_tree table (Pear_parse.Lambda_list [] trees))
                (Symbol {
                    pear_type: Unit,
                    position: {line: -1, column: -1},
                    data: "", /* it's a unit */
                })
        }
    }
};

let get_type tree => {
    switch tree {
        | Symbol t_unit => {
            t_unit.pear_type
        }
        | Function_call a_type function_itself argument => {
            a_type
        }
        | Function_definition a_type t_unit trees => {
            a_type
        }
    }
};

let print_constraints constraints =>
    List.map (fun (a, b) => {
        print_string ("\t" ^ (string_of_pear_type a) ^ " == " ^ (string_of_pear_type b) ^ "\n")
    }) constraints;

let print_substitutions constraints =>
    List.map (fun (a, b) => {
        print_string ("\t" ^ string_type_of_int a ^ " == " ^ string_of_pear_type b ^ "\n")
    }) constraints;


let rec collect_constraints tree : list 'a => {
    let output = ref [];
    let constrain argument =>
        output := [argument, ...!output];
    switch tree {
        | Function_call a_type function_itself argument => {
            /* keep the constraints for both the function and its argument */
            List.iter constrain (collect_constraints argument);
            List.iter constrain (collect_constraints function_itself);

            /* the function should be of type function(argument_type)return_type */
            constrain (get_type function_itself, Function (get_type argument) a_type);
        }
        | Function_definition a_type t_unit trees => {
            /* iterate over the trees and keep their constraints */
            List.iter (fun tree => List.iter constrain (collect_constraints tree)) trees;

            /* implicit return from a list of expressions */
            let last_argument = List.nth trees (List.length trees - 1);
            constrain (a_type, Function t_unit.pear_type (get_type last_argument));
        }
        | Symbol t_unit => ()
    };
    !output
};

let rec occurs a t => switch t {
    | Generic b => a == b;
    | Function u v => occurs a u || occurs a v;
    | _ => false;
};

let rec substitute x replacement pear_type => {
    switch pear_type {
        /* actually substitute here */
        | Generic y => if (x == y) { replacement } else { pear_type }
        | Function a b => Function (substitute x replacement a) (substitute x replacement b)
        | _ => pear_type
    }
};

let apply_to_type substitutions typ => {
    List.fold_right (fun (x, e) z => substitute x e z ) substitutions typ
};

let apply_to_tree substitutions tree => {

    let apply_typed_unit {pear_type, position, data} (x, typ) => {
        {
            pear_type: substitute x typ pear_type,
            position,
            data,
        }
    };

    let rec apply_tree (x, typ) tree => {
        switch tree {
            | Symbol tunit => {
                Symbol
                    (apply_typed_unit tunit (x, typ))
            }
            | Function_call ptype t1 t2 => {
                Function_call
                    (substitute x typ ptype)
                    (apply_tree (x, typ) t1)
                    (apply_tree (x, typ) t2)
            }
            | Function_definition ptype tunit ts =>
                Function_definition
                    (substitute x typ ptype)
                    (apply_typed_unit tunit (x, typ))
                    (List.map (apply_tree (x, typ)) ts)
        }
    };
    
    List.fold_right apply_tree substitutions tree
};

let rec unify constraints => {
    let rec unify_one t1 t2 => {
        switch (t2, t1) {
            | (Generic x, Generic y) => if (x == y) { [] } else { [(x, t2)] };
            | (Function a b, Function c d) => unify [(a, c), (b, d)];
            | (Function a b as z, Generic x) | (Generic x, Function a b as z) =>
               if (occurs x z) {
                    raise (Pear_utils.empty_pear_bug "not unifiable - circularity")
               } else {
                    [(x, z)]
               };
            | (Generic x, a) | (a, Generic x) => [(x, a)]; /* I'm not sure about this */
            | (a, b) => []; /* or this */
        }
    };

    if (List.length constraints == 0) {
        []
    } else {
        let (x, y) = List.hd constraints;
        let unified_0 = unify (List.tl constraints);
        let unified_1 = unify_one (apply_to_type unified_0 x) (apply_to_type unified_0 y);
        List.append unified_0 unified_1
    }
};

let infer_all tree respect_outer => {
    let tree = convert_to_typed_tree (Hashtbl.create 16) tree;

    switch tree {
        | Function_call _ (Function_definition _ _ body) _ => {
            let tree = if respect_outer { tree } else { List.hd body };
            let constraints = collect_constraints tree;
            let substitutions = unify constraints;
            let treetype = apply_to_type substitutions (get_type tree);

            /*
            print_string (string_of_typed_tree tree);
            print_string "constraints:\n";
            print_constraints constraints;
            print_string "\nsubstitutions:\n";
            print_substitutions substitutions;
            */

            let tree = apply_to_tree substitutions tree;

            (tree, constraints, treetype)
        }
        | _ => raise (Pear_utils.empty_pear_bug "the way it's set up this won't happen")
    }
};

let infer_for_test tree => {
    let (tree, _, _) = infer_all tree false;
    tree
};

let infer tree => {
    let (tree, _, _) = infer_all tree true;
    tree
};
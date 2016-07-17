/* silver type */

open Silver_utils;

open Silver_token;

open Silver_parse;

/*

 Table of Contents

 Section 1. Functions for and about Silver Trees (The typed tree used for unification)
 Section 2. Functions for making constraints which will eventually be unified
 Section 3. Functions for doing the actual unification

 */
/* Section 1: Silver Trees */
/* These three types make up all possible types in silver */
type attribute = (string, silver_type)
and silver_type =
  | Generic of int
  | Unit
  | Function of silver_type silver_type
  | Integer
  | Float
  | Object of (list attribute)
  | Open_object of (list attribute);

type generic_silver_tree 'a =
  | Symbol of silver_type 'a
  | Function_def of silver_type (generic_silver_tree 'a) (list (generic_silver_tree 'a))
  | Function_app of silver_type (generic_silver_tree 'a) (generic_silver_tree 'a);

/* This is a concrete tree without type parameters */
type silver_tree = generic_silver_tree (Silver_token.token, Silver_utils.position);

type substitution = {id: int, silver_type: silver_type};

let rec string_of_silver_type silver_type =>
  switch silver_type {
  | Unit => "unit"
  | Generic i => Silver_utils.string_type_of_int i
  | Function arg ret => "(" ^ string_of_silver_type arg ^ "->" ^ string_of_silver_type ret ^ ")"
  | Integer => "int"
  | Float => "float"
  | Object _ => "object"
  | Open_object _ => "open_object"
  };

let rec string_of_silver_tree tree =>
  switch tree {
  | Symbol silver_type (a, _) =>
    Silver_token.string_of_token a ^ "::" ^ string_of_silver_type silver_type
  | Function_def silver_type argument body =>
    "{ " ^
      string_of_silver_tree argument ^
      " : " ^
      String.concat "; " (List.map string_of_silver_tree body) ^
      "}::" ^
      string_of_silver_type silver_type
  | Function_app silver_type argument ret =>
    "(" ^
      string_of_silver_tree argument ^
      " " ^
      string_of_silver_tree ret ^
      ")::" ^
      string_of_silver_type silver_type
  };

/* generate a different generic type each time */
let symbol_tracker = ref 0;

let reset_count () => symbol_tracker := 0;

let generic_type () => {
  let a = !symbol_tracker;
  symbol_tracker := a + 1;
  Generic a
};

/* this function converts an abstract tree to a silver tree (with types) */
let unit_token = Symbol Unit (Silver_token.Unit, {line: 0, column: 0});

let rec initial_to_silver abstract_tree =>
  switch abstract_tree {
  | Silver_parse.Symbol s => Symbol (generic_type ()) s
  | Silver_parse.Call_list lst =>
    switch (List.length lst) {
    | 0 => raise (Silver_utils.empty_silver_bug "a call list cannot have 0 arguments!")
    | 1 => initial_to_silver (List.hd lst)
    | 2 =>
      Function_app
        (generic_type ()) (initial_to_silver (List.hd lst)) (initial_to_silver (List.nth lst 1))
    | n =>
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
        [
          initial_to_silver (
            switch (List.length body) {
            | 0 =>
              raise (
                Silver_utils.empty_silver_bug "a function definition needs at least one item in body"
              )
            | n => Silver_parse.Lambda_list (List.tl arguments) body
            }
          )
        ]
    }
  | Silver_parse.Sequence_list body => initial_to_silver (Silver_parse.Lambda_list [] body)
  };

let type_of_silver_tree tree =>
  switch tree {
  | Symbol silver_type _ => silver_type
  | Function_app silver_type action argument => silver_type
  | Function_def silver_type argument body => silver_type
  };

/* Section 2: Constraints */
let rec map_over_tree fapp fdef fsymbol (tree: silver_tree) =>
  switch tree {
  | Function_app silver_type action argument =>
    let this_round =
      switch fapp {
      | None => []
      | Some f => f silver_type action argument
      };
    List.append
      (List.concat (List.map (map_over_tree fapp fdef fsymbol) [action, argument])) this_round
  | Function_def silver_type argument body =>
    let (this_round, callback) =
      switch fdef {
      | None => ([], None)
      | Some f => f silver_type argument body
      };
    let constraints =
      List.append (List.concat (List.map (map_over_tree fapp fdef fsymbol) body)) this_round;
    /* call the callback if it exists */
    /* this is for some unfortunate stateful trickery */
    switch callback {
    | None => ()
    | Some f => f ()
    };
    constraints
  | Symbol silver_tree a =>
    switch fsymbol {
    | None => []
    | Some f => f silver_tree a
    }
  };

let constrain_function_bindings tree => {
  let table = Hashtbl.create 16;
  map_over_tree
    /* function app */
    None
    /* function def */
    (
      Some (
        fun silver_type arg body => {
          /* add the argument's type to the hashtable */
          let old_silver_type =
            switch arg {
            | Symbol silver_type (Identifier ident, position) =>
              let old_type_info =
                switch (Hashtbl.find table ident) {
                | t => Some (ident, t)
                | exception Not_found => None
                };
              Hashtbl.add table ident silver_type;
              old_type_info
            | _ => None
            };
          let callback () =>
            switch old_silver_type {
            | None => ()
            | Some (ident, old_type) => Hashtbl.add table ident old_type
            };
          ([], Some callback)
        }
      )
    )
    /* symbol */
    (
      Some (
        fun silver_type (token, position) =>
          switch token {
          | Identifier ident =>
            switch (Hashtbl.find table ident) {
            | a_type => [(a_type, silver_type)]
            | exception Not_found => []
            }
          | _ => []
          }
      )
    )
};

let constrain_function_calls =
  map_over_tree
    /* function app */
    (
      Some (
        fun silver_type action argument => [
          (type_of_silver_tree action, Function (type_of_silver_tree argument) silver_type)
        ]
      )
    )
    /* function def */
    None
    /* symbol */
    None;

let constrain_function_definitions =
  map_over_tree
    /* function app */
    None
    /* function def */
    (
      Some (
        fun silver_type argument body => (
          [
            (
              silver_type,
              Function
                (type_of_silver_tree argument) (type_of_silver_tree (Silver_utils.last_of body))
            )
          ],
          None
        )
      )
    )
    /* symbol */
    None;

let string_of_constraints constraints =>
  String.concat
    ""
    (
      List.map
        (
          fun (t1, t2) =>
            " - " ^ string_of_silver_type t1 ^ " <=> " ^ string_of_silver_type t2 ^ "\n"
        )
        constraints
    );

let constrain_integer_types =
  map_over_tree
    None
    None
    (
      Some (
        fun silver_type (token, position) =>
          switch token {
          | Identifier data =>
            switch (int_of_string data) {
            | i => [(silver_type, Integer)]
            | exception Failure "int_of_string" => []
            }
          | _ => []
          }
      )
    );

/* Section 3: Unification */
/* apply substitutions to a type */
let apply (subs: list substitution) silver_type :silver_type => {
  /* substitute silver_type for all occurences of id in parent_type */
  let rec substitute id silver_type parent_type =>
    switch parent_type {
    | Generic y =>
      if (id == y) {
        silver_type
      } else {
        parent_type
      }
    | Function arg_type ret_type =>
      Function (substitute id silver_type arg_type) (substitute id silver_type ret_type)
    | Unit => Unit /* don't substitute anything */
    | Integer => Integer /* don't substitute anything */
    | _ => raise (Silver_utils.empty_silver_bug "haven't gotten to these types yet")
    };
  /* this list.rev solves a bug that was replacing 'c with ('b -> 'a),
     after 'b had been replaced with 'a; this might return in the future */
  List.fold_right (fun {id, silver_type} => substitute id silver_type) (List.rev subs) silver_type
};

/* unify a list of pairs */
let rec unify constraints :list substitution =>
  switch constraints {
  | [] => []
  | [(x, y), ...rest] =>
    let rest_sub = unify rest;
    let this_sub = unify_one (apply rest_sub x) (apply rest_sub y);
    List.append rest_sub this_sub
  }
/* does id occur in silver_type? */
and occurs id silver_type =>
  switch silver_type {
  | Generic y => y == id
  | Function arg_type ret_type => occurs id arg_type || occurs id ret_type
  | Unit => false
  | Integer => false
  | _ => raise (Silver_utils.empty_silver_bug "haven't gotten to these types yet")
  }
/* unify one pair */
and unify_one type_1 type_2 =>
  switch (type_1, type_2) {
  /*
   * Generic <=> Generic */
  | (Generic x, Generic y as z) =>
    if (x == y) {
      []
    } else {
      [{id: x, silver_type: z}]
    }
  /*
   * Function <=> Function */
  | (Function arg1 ret1, Function arg2 ret2) => unify [(arg1, arg2), (ret1, ret2)]
  /*
   * Generic <=> Function */
  | (Generic x, Function arg ret as z)
  | (Function arg ret as z, Generic x) =>
    if (occurs x z) {
      raise (Silver_utils.empty_silver_error "not unifiable: circularity")
    } else {
      [{id: x, silver_type: z}]
    }
  /*
   * Integer <=> Generic */
  | (Generic x, Integer)
  | (Integer, Generic x) => [{id: x, silver_type: Integer}]
  /*
   * Anything else is an error */
  | _ => raise (Silver_utils.empty_silver_bug "haven't gotten to these types yet")
  };

let rec map_over_type function_for_type silver_tree =>
  switch silver_tree {
  | Function_app silver_type arg ret =>
    Function_app
      (function_for_type silver_type)
      (map_over_type function_for_type arg)
      (map_over_type function_for_type ret)
  | Function_def silver_type arg body =>
    Function_def
      (function_for_type silver_type)
      (map_over_type function_for_type arg)
      (List.map (map_over_type function_for_type) body)
  | Symbol silver_type a => Symbol (function_for_type silver_type) a
  };

let apply_to_tree substitutions silver_tree => map_over_type (apply substitutions) silver_tree;

let convert_to_silver_tree abstract_tree => {
  let silver_tree = initial_to_silver abstract_tree;
  let constraints = [];
  let constraints = constrain_function_bindings constraints silver_tree;
  let constraints = List.append constraints (constrain_function_calls silver_tree);
  let constraints = List.append constraints (constrain_function_definitions silver_tree);
  let constraints = List.append constraints (constrain_integer_types silver_tree);
  let substitutions = unify constraints;
  let silver_tree = apply_to_tree substitutions silver_tree;
  (silver_tree, constraints)
};

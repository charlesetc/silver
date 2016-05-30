/* apricot parse */

open Apricot_utils;
open Apricot_token;

/*

 Valid expressions:

 x

 { x ; y }

 { x y :  ... ; ... }

 /* lambda (x y) {... ; ...} */

 */

exception Stop_iteration;

type abstract_tree 'a
  = Symbol of 'a
  /* used for function call notation */
  | Call_list of (list (abstract_tree 'a))
  /* used for sequence => { ... ; ... } */
  | Sequence_list of (list (abstract_tree 'a))
  ;


let add_to o x => o := [x, ...!o];

let rec string_of_abstract_tree tree => {

  let output = ref "";

  let add_string s => output := !output ^ s;
  let add_char c => add_string (Apricot_utils.string_of_char c);
    
  switch tree {
    | Symbol (token, _) => {
      add_char ' ';
      add_string (Apricot_token.string_of_token token);
      add_char ' ';
    }
    | Call_list list_of_trees => {
      add_char '(';
      List.iter 
        (fun x => {
          add_string (string_of_abstract_tree x);
        })
        list_of_trees;
      add_char ')';
    }
    | Sequence_list list_of_trees => {
      add_string "{ ";
      List.iter
        (fun x => {
          add_string (string_of_abstract_tree x);
          add_string "; ";
        })
        list_of_trees;
      add_string "} ";
    }
  };

  !output
};


let parse stream => {

  let rec add_parentheses => {
    let output = ref [];

    let add_to_output x => output := [x, ...!output];

    let iterate => Stream.iter (fun item => {
      switch item {
        | (Apricot_token.Left_round, _) =>
          add_to_output (Call_list (add_parentheses ()));
        | (Apricot_token.Right_round, _) => raise Stop_iteration;
        | data => add_to_output (Symbol data)
      }
    }) stream;

    /* uses stop iteration exception as break */
    switch (iterate ()) {
      | _ => ();
      | exception Stop_iteration => ();
    };

    !output
  };

  let rec add_sequences stream => {
    let output = ref [];
    let outgoing_item = ref [];

    let next_item => {
      if (List.length !outgoing_item != 0) {
        add_to output (Call_list !outgoing_item);
        outgoing_item := [];
      }
    };

    let iterate => Stream.iter (fun tree => {
      switch tree {
        | Symbol (Apricot_token.Left_curly, _) =>
          add_to outgoing_item (Sequence_list (add_sequences stream));
        | Symbol (Apricot_token.Right_curly, _) => {
          next_item ();
          raise Stop_iteration;
        }
        | Symbol (Apricot_token.Newline, _) => {
          next_item ();
        }
        | Symbol data => add_to outgoing_item (Symbol data);
        | Call_list trees => add_to outgoing_item (Call_list (add_sequences (Stream.of_list trees)));
        | Sequence_list trees =>
          raise (Apricot_utils.Apricot_bug
            "there shouldn't be sequences at this stage"
            {line: -1, column: -1});
      }
    }) stream;

    /* uses stop iteration exception as break */
    switch (iterate ()) {
      | _ => ();
      | exception Stop_iteration => ();
    };

    next_item ();

    !output
  };

  let rec remove_spaces trees => {
    let output = ref [];

    List.iter (fun tree => {
      switch tree {
        | Symbol (Apricot_token.Space, _) => ();
        | Symbol data => add_to output (Symbol data);
        | Call_list trees => add_to output (Call_list (remove_spaces trees));
        | Sequence_list trees => add_to output (Sequence_list (remove_spaces trees));
      }
    }) trees;
    !output
  };

  let rec reverse_everything tree => {
    switch tree {
      | Call_list trees => {
        let trees = List.map reverse_everything trees;
        Call_list (List.rev trees)
      }
      | Sequence_list trees => {
        let trees = List.map reverse_everything trees;
        Sequence_list (List.rev trees)
      }
      | all => all;
    }
  };

  let rec simplify_single_parentheses tree => {
    switch tree {
      | Call_list trees when List.length trees == 1 =>
          simplify_single_parentheses (List.nth trees 0);
      | Call_list trees => {
        let trees = List.map simplify_single_parentheses trees;
        Call_list (List.rev trees)
      }
      | Sequence_list trees => {
        let trees = List.map simplify_single_parentheses trees;
        Sequence_list (List.rev trees)
      }
      | all => all;
    }
  };

  let state = add_parentheses ();

  let state = remove_spaces state;

  let state = add_sequences (Stream.of_list state);

  let state = Sequence_list state;

  let state = simplify_single_parentheses state;

  /* let state = reverse_everything state; */

  state
};

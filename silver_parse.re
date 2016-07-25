/* silver parse */

open Silver_utils;

open Silver_token;

/*

 Valid expressions:

 x

 { x ; y }

 { x y :  ... ; ... }

 /* lambda (x y) {... ; ...} */

 */
exception Stop_iteration;

type abstract_tree 'a =
  | Symbol of 'a
  /* used for function call notation */
  | Call_list of (list (abstract_tree 'a))
  /* used for sequence => { ... ; ... } */
  | Sequence_list of (list (abstract_tree 'a))
  /* used for lambdas => { x y : ... ; ... } */
  | Lambda_list of (list (abstract_tree 'a)) (list (abstract_tree 'a))
  /* used for struct literals => {1: 2, 3: 4} */
  | Struct_list of (list (abstract_tree 'a));

let add_to o x => o := [x, ...!o];

let bad_add_to o x => o := List.append !o [x];

let is_dot_tree tree =>
  switch tree {
  | Symbol (Dot_literal _, _) => true
  | _ => false
  };

let rec string_of_abstract_tree tree => {
  let output = ref "";
  let add_string s => output := !output ^ s;
  let add_char c => add_string (Silver_utils.string_of_char c);
  switch tree {
  | Symbol (token, _) =>
    add_char ' ';
    add_string (Silver_token.string_of_token token);
    add_char ' '
  | Call_list list_of_trees =>
    add_char '(';
    List.iter (fun x => add_string (string_of_abstract_tree x)) list_of_trees;
    add_char ')'
  | Struct_list struct_map =>
    add_char '<';
    List.iter (fun value => add_string (string_of_abstract_tree value)) struct_map;
    add_char '>';
  | Sequence_list list_of_trees =>
    add_string "{ ";
    List.iter
      (
        fun x => {
          add_string (string_of_abstract_tree x);
          add_string "; "
        }
      )
      list_of_trees;
    add_string "} "
  | Lambda_list first_list second_list =>
    add_string "{lambda ";
    List.iter
      (
        fun x => {
          add_string (string_of_abstract_tree x);
          add_string " "
        }
      )
      first_list;
    add_string "of ";
    List.iter
      (
        fun x => {
          add_string (string_of_abstract_tree x);
          add_string "; "
        }
      )
      second_list;
    add_string "} "
  };
  !output
};

let rec add_dot_syntax ast => {
  let rec add_dot_calls list =>
    switch list {
    | [x, y, ...xs] =>
      if (is_dot_tree y) {
        add_dot_calls [Call_list [y, x], ...xs]
      } else {
        [x, ...add_dot_calls [y, ...xs]]
      }
    | xs => xs
    };
  switch ast {
  | Symbol a => Symbol a
  | Call_list list =>
    /* let list = switch_method_syntax list; */
    let list = add_dot_calls list;
    Call_list (List.map add_dot_syntax list)
  | Sequence_list list => Sequence_list (List.map add_dot_syntax list)
  | Struct_list list => Struct_list (List.map add_dot_syntax list)
  | Lambda_list args body =>
    Lambda_list (List.map add_dot_syntax args) (List.map add_dot_syntax body)
  }
};

let parse stream => {
  let rec add_parentheses () => {
    let output = ref [];
    let iterate () =>
      Stream.iter
        (
          fun item =>
            switch item {
            | (Silver_token.Left_round, _) => add_to output (Call_list (add_parentheses ()))
            | (Silver_token.Right_round, _) => raise Stop_iteration
            | data => add_to output (Symbol data)
            }
        )
        stream;
    /* uses stop iteration exception as break */
    switch (iterate ()) {
    | _ => ()
    | exception Stop_iteration => ()
    };
    !output
  };
  let rec add_angle_parentheses stream => {
       let output = ref [];
         let iterate () =>
             Stream.iter (
                 fun tree => switch tree {
                     | Symbol (Silver_token.Left_angle, position) =>
                        let tokens = add_angle_parentheses stream;
                         add_to output (Struct_list tokens);
                     | Symbol (Silver_token.Right_angle, _) =>
                         raise Stop_iteration
                     | Symbol _ => add_to output tree
                     | Call_list trees =>
                         add_to output (Call_list (add_angle_parentheses (Stream.of_list trees)))
                     | Sequence_list trees =>
                        add_to output (Sequence_list (add_angle_parentheses (Stream.of_list trees)))
                     | _ =>
                       raise (Silver_utils.empty_silver_bug "there shouldn't be lambdas or structs at this stage")
                 }
             )
             stream;

         /* uses stop iteration exception as break */
         switch (iterate ()) {
         | _ => ()
         | exception Stop_iteration => ()
         };
         List.rev !output
     };
  let rec add_sequences stream => {
    let output = ref [];
    let outgoing_item = ref [];
    let next_item () =>
      if (List.length !outgoing_item != 0) {
        add_to output (Call_list !outgoing_item);
        outgoing_item := []
      };
    let iterate () =>
      Stream.iter
        (
          fun tree =>
            switch tree {
            | Symbol (Silver_token.Left_curly, _) =>
              add_to outgoing_item (Sequence_list (add_sequences stream))
            | Symbol (Silver_token.Right_curly, _) =>
              next_item ();
              raise Stop_iteration
            | Symbol (Silver_token.Newline, _) => next_item ()
            | Symbol data => add_to outgoing_item (Symbol data)
            | Call_list trees =>
              add_to outgoing_item (Call_list (add_sequences (Stream.of_list trees)))
            | Struct_list trees =>
            let with_sequences = List.map (fun x => add_sequences (Stream.of_list [x])) trees;
                add_to outgoing_item (Struct_list (List.concat with_sequences));
            | Sequence_list _ =>
              raise (Silver_utils.empty_silver_bug "there shouldn't be sequences at this stage")
            | Lambda_list _ _ =>
              raise (Silver_utils.empty_silver_bug "there shouldn't be lambdas at this stage")
            }
        )
        stream;
    /* uses stop iteration exception as break */
    switch (iterate ()) {
    | _ => ()
    | exception Stop_iteration => ()
    };
    next_item ();
    !output
  };
      let remove_top_token token => List.filter (fun x => switch x {
          | Symbol (this_token, position) =>
          this_token != token
          | _ =>
           true
      });
  let rec remove_punctuation_from_structs tree => {
      switch tree {
          | Symbol _ => tree
          | Struct_list rest =>
              let rest = remove_top_token Silver_token.Colon rest;
              let rest = remove_top_token Silver_token.Comma rest;
              Struct_list rest
          | Call_list list => Call_list (List.map remove_punctuation_from_structs list);
          | Sequence_list list => Sequence_list (List.map remove_punctuation_from_structs list);
          | _ =>
            raise (
              Silver_utils.Silver_bug
                "there shouldn't be lambdas at this stage" {line: (-1), column: (-1)}
            )
      }
  };
  let rec remove_spaces trees => {
    let output = ref [];
    List.iter
      (
        fun tree =>
          switch tree {
          | Symbol (Silver_token.Space, _) => ()
          | Symbol data => add_to output (Symbol data)
          | Call_list trees => add_to output (Call_list (remove_spaces trees))
          | Struct_list trees => add_to output (Struct_list (remove_spaces trees))
          | Sequence_list trees => add_to output (Sequence_list (remove_spaces trees))
          | Lambda_list _ _ =>
            raise (
              Silver_utils.Silver_bug
                "there shouldn't be lambdas at this stage" {line: (-1), column: (-1)}
            )
          }
      )
      trees;
    !output
  };
  let rec reverse_everything tree =>
    switch tree {
    | Call_list trees =>
      let trees = List.map reverse_everything trees;
      Call_list (List.rev trees)
    | Struct_list trees =>
      let trees = List.map reverse_everything trees;
      Struct_list (List.rev trees)
    | Sequence_list trees =>
      let trees = List.map reverse_everything trees;
      Sequence_list (List.rev trees)
    | all => all
    };
  let rec simplify_single_parentheses tree =>
    switch tree {
    | Call_list trees when List.length trees == 1 => simplify_single_parentheses (List.hd trees)
    | Call_list trees =>
      let trees = List.map simplify_single_parentheses trees;
      Call_list (List.rev trees)
    | Struct_list trees  =>
      let trees = List.map simplify_single_parentheses trees;
      Struct_list (trees)
    | Sequence_list trees =>
      let trees = List.map simplify_single_parentheses trees;
      Sequence_list (List.rev trees)
    | all => all
    };
  let rec add_lambdas tree => {
    let find_colon_index tree => {
      let rec find_index list =>
        switch (List.hd list) {
        /* the exception is caught later and
           interpreted as false */
        | Symbol (Silver_token.Colon, _) => 0
        | _ => find_index (List.tl list) + 1
        };
      switch tree {
      | Call_list arguments => Some (find_index arguments)
      | _ => None
      }
    };
    let transform_lambda trees index => {
      let arguments =
        switch (List.hd trees) {
        | Call_list arguments => arguments
        | _ =>
          raise (
            Silver_bug
              "arguments to a lambda should always be a call_list" {line: (-1), column: (-1)}
          )
        };
      let (first_arguments, second_arguments) = Silver_utils.split_at index arguments;
      /* get rid of the colon */
      let second_arguments = List.tl second_arguments;
      let tail = List.tl trees;
      let second_arguments =
        switch (List.length second_arguments) {
        | 0 => tail
        | 1 => [List.hd second_arguments, ...tail]
        | _ => [Call_list second_arguments, ...tail]
        };
      Lambda_list first_arguments second_arguments
    };
    switch tree {
    | Call_list trees =>
      let trees = List.map add_lambdas trees;
      Call_list trees
    | Struct_list trees =>
        let trees = List.map add_lambdas trees;
        Struct_list trees
    | Lambda_list first_trees second_trees =>
      raise (
        Silver_utils.Silver_bug
          "there shouldn't be lambdas at this stage" {line: (-1), column: (-1)}
      )
    /* { */
    /* let second_trees = List.map add_lambdas second_trees; */
    /* Lambda_list first_trees second_trees */
    /* } */
    | Sequence_list trees =>
      let trees = List.map add_lambdas trees;
      switch (find_colon_index (List.hd trees)) {
      | Some index => transform_lambda trees index
      | None => Sequence_list trees
      | exception Failure _ => Sequence_list trees
      }
    | all => all
    }
  };
  /* TODO: make list reversing not a problem with this file. */
  /* Right now it's very ugly and unintelligent - if we had */
  /* doubly linked lists this would be trivial */
  let state = add_parentheses ();
  let state = remove_spaces state;
  let state = add_angle_parentheses (Stream.of_list state);
  let state = add_sequences (Stream.of_list state);
  let state = Sequence_list state;
  let state = simplify_single_parentheses state;
  /*
  let state = reverse_everything state;
  */
  let state = remove_punctuation_from_structs state;
  let state = simplify_single_parentheses state;
  let state = reverse_everything state;
  let state = add_lambdas state;
  let state = add_dot_syntax state;
  let state = simplify_single_parentheses state;
  let state = reverse_everything state;
  state
};

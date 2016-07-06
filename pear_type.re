/* pear type */

open Pear_utils;

/* These three types make up all possible types in pear */

type attribute = (string, pear_type)

and small_type = Unit
                | Integer
                | Float
                | Function of pear_type pear_type
                | Object of (list attribute)

and pear_type = Generic of int
               | Abstract of small_type
               | Concrete of small_type
               ;

type generic_pear_tree 'a = Symbol of pear_type 'a
                          | Function_def of pear_type (generic_pear_tree 'a, list (generic_pear_tree 'a))
                          | Function_app of pear_type (generic_pear_tree 'a, generic_pear_tree 'a)
                          ;

/* This is a concrete tree without type parameters */
type pear_tree = generic_pear_tree (string, Pear_utils.position);


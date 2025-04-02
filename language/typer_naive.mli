(** This module contains the weak polymorphic typer of  mini-ml language.

You should start here.

*)

val type_expr :
  Typer_util.Counter.t ->
  Ast.type_lang Util.Environment.t ->
  Ast.expr ->
  Ast.type_lang * (Ast.type_lang * Ast.type_lang) list
(** The main function that types expressions.
It does two tasks: associating types to each nodes, and collecting type constraints implied by any case.

The base case are straightforward, and generate no constraint (see {!Typer_util.type_of_built_in} for types of buit-in functions).

Only cases {!Ast.App} and {!Ast.IfThenElse} add new constraints, the other inductive type simply append the constraints of their sub-expressions.

Every time a new name is introduced ({!Ast.Fun} and {!Ast.Let} (only if it is recursive)), you have to create a fresh universal type (with the {!Typer_util.Counter.t} provided) and add it to the environment (associate it with name). For the {!Ast.Let}, when evaluating the second expression you also have to bind the name to the type of the first expression.
In all this cases, it is necessary to remove the name from the environment when it falls out of scope, i.e., at the end of the typing of that node. Be careful for the {!Ast.Let} (when it is recursive, you have to add twice a type to name, and therefore to remove the association twice).
*)

(** This module contains the full polymorphic typer of mini-ml language.

Do NOT start by that. First do the weak polymorphic typer. What you'll have to do here is very similar, with a few adaptations.

*)

val type_expr :
  Typer_util.Counter.t ->
  Ast.type_lang Util.Environment.t ->
  Ast.expr ->
  Ast.type_lang * (Ast.type_lang * Ast.type_lang) list
(** The main function that types expression. 

It should be very similar to {!Typer_naive.type_expr}.
The only differences are for the {!Ast.App} case (you have to instantiate the types of the two expressions) and the {!Ast.Let} case (you have to solve the local constraints after the evaluation of the expression defined, and to generalize the obtained expression).
*)

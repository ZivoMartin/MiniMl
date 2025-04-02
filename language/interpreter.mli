(** Module that contains an interpreter of mini-ml. *)

(** Type of values being manipulated by a mini-ml program. *)
type value =
  | Int of int
  | Bool of bool
  | String of string
  | Unit
  | Func of (value -> value)
  | List of value list

val pp_value : Format.formatter -> value -> unit
(** Pretty-printer for a {!value} *)

val func_of_built_in : Ast.built_in -> value
(** Function that defines the value corresponding to a built-in function*)

val interpret_expression : value Util.Environment.t -> Ast.expr -> value
(** Main function that computes the value associated to a mini-ml expression in a given environment. *)

val interpret_program : value Util.Environment.t -> Ast.req list -> unit
(** Function that interprets a list of requests. *)

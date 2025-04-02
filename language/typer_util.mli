(**
    This module contains function that are useful to the different typers we will implement, and that you'll have to implement
*)

exception Constraint_error of Ast.type_lang * Ast.type_lang
(** Exception that can be returned when a constraint cannot be solved -- it should be catched where you call {!solve_constraints}*)

exception Typing_error of Util.Position.t * string
(** Exception that can be returned in case a typing error occurs.
The position is the position of the term in which the error occurs (for pretty-printing), and the string is an explanation of the error returned. *)

(** Technical module to represent a counter that can create fresh values when needed. Used for creating new universal types when needed *)
module Counter : sig
  type t

  val create : unit -> t
  (** Returns a counter initialized to 0*)

  val get_fresh : t -> int
  (** Gets the next value of the counter and increments it. A counter will never return twice the same value. *)
end

val type_of_built_in : Ast.built_in -> Ast.type_lang
(** Function that returns the type of every built-in function of mini-ml. See {!Ast} for the type to put here. *)

val solve_constraints :
  (Ast.type_lang * Ast.type_lang) list -> (int * Ast.type_lang) list
(** Function that solves a list of constraints and produces the resulting substitution. Treats each constraint sequentially. The principle is the following : if a constraint is not solvable, raises a {!Constraint_error} ; otherwise, compute the corresponding substitution (see below), apply it to the rest of the list of constraints, and then solve them.
    The solvable constraints are :
    - (TUniv n,t) or (t,TUniv n) : only if t does not contain TUniv n. In that case, the substitution is (n,t). If t is TUniv m, we replace the bigger variable with the smaller one.
    - (TFunc(_,arg1,res1),TFunc(_,arg2,res2)). In that case, we do not have a direct substitution, but we need to solve in addition the constraints (arg1,arg2) and (res1,res2).
    - TList(_,t1),TList(_t2). Similarly, here we have to solve the constraint (t1,t2).
    - (t1,t2). If t1 = t2, the constraint is solvable, and we have no substitution to add (as there are constant types) to the result returned by the remainder of the constraints. If not, this is an error case.
    *)

val instantiate : Counter.t -> Ast.type_lang -> Ast.type_lang
(** [instantiate counter target] replace any universal type that are generalized in [target] with fresh (unused) universal type. For example, if we have [instantiate counter (TFunc([1],TUniv 0,TUniv 1))], it will return [TFunc([],TUniv 0,TUniv n)] where [n] will be the value returned by [counter].*)

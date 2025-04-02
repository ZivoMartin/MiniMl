(** This module defines a lot of utilitary functions used to deal with types that are provided to (and hidden from) you. You will need these functions for implementing the different versions of the typer.*)

val get_type_generic : Ast.type_lang -> int list
(** Get the list of generalized universal type of a type, i.e., the list associated to a type {!Ast.TFunc} or {!Ast.TList}. Empty for all other types. For example, [get_type_generic (TFunc([1],TUniv 1,TUniv 0))] will return [[1]].

(I don't need it in my implementation, but should you need it, I leave it available).*)

val get_free_type_var : Ast.type_lang -> int list
(** Get the list of universal type that are unbound by a generalization. For example [get_free_type_var (TFunc([1],TUniv 1, TUniv 0))] will return [[0]], as [1] is bound within the type.

This function is useful for implementing {!Typer_util.solve_constraints}(when you need to determine whether a constraint of the form [TUniv(n),t]) is solvable).*)

val apply_subst_in_type :
  (int * Ast.type_lang) list -> Ast.type_lang -> Ast.type_lang
(** [apply_subst_in_type subst target] will successively apply all substitution in [subst] (with [substitute_univ_in_type] to [target] and return the result).

For example, [apply_subst_in_type [(0,TUniv 1);(1,TInt)] (TFunc([],TFunc([],TUniv 0,TUniv 2),TUniv 1))] will return [TFunc([],TFunc([],TInt,TUniv 2),TInt)].
*)

val substitute_constraint :
  int ->
  Ast.type_lang ->
  Ast.type_lang * Ast.type_lang ->
  Ast.type_lang * Ast.type_lang
(** [substitute_constraint num typ (c1,c2)] replaces any occurrence of [TUniv(num)] with [t] in [c1] and [c2] and returns the resulting constraint. (similarly as what is above)

This function will only be useful when implementing {!Typer_util.solve_constraints}.
*)

val split_constraint_by_floor :
  int ->
  (Ast.type_lang * Ast.type_lang) list ->
  (Ast.type_lang * Ast.type_lang) list * (Ast.type_lang * Ast.type_lang) list
(** [split_constraint_by_floor floor list] will separate the list of constraints [list] into [inner,outer], [inner] being the constraints who contain some occurrence of universal variable higher than floor or no universal variable, and [outer] the constraint that concern some universal variable strictly lower than [floor].

  [inner] are the constraints that should be solved in a let declaration, while [outer] are those who should be solved in the containing expression (because they will have impact on those).

This function will only be useful for the fully polymorphic typer.*)

val type_substitution_in_expr : Ast.expr -> (int * Ast.type_lang) list -> unit
(** [type_substitution_in_expr expr substitution] applies the type substitution [substitution] to all annotations present in [expr]. If [expr] is not already typed, does nothing.

For example, if the substitution contains [(1,TInt)], it will replace any occurrence of type [TUniv 1] with [TInt].

You will only have to use it explicitly for the fully polymorphic typer (for the weak one, it is called for you in main).
  *)

val generalize_type_expr : int -> Ast.expr -> Ast.type_lang
(** [generalize_type_expr floor expr] generalizes every universal variable of index higher than [floor] present in the type annotating [expr].
Expects [expr] to have a type annotation, and will raise an exception if it is not the case.

You will only have to use it for the fully polymorphic typer (for the weak one, it is not useful).
  *)

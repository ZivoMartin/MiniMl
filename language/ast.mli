(** This module defines the language mini-ml that we compile in this project, alongside some display functions, annotations containing parsing and type information, and functions used for applying type substitution to an already typed expression (useful for the typer)*)

(** Type representing the possible types of a mini-ml expression *)
type type_lang =
  | TInt  (** int type *)
  | TBool  (** bool type *)
  | TString (*** string type *)
  | TUnit  (** unit type (action) *)
  | TFunc of int list * type_lang * type_lang
      (** function type. Functions have only one parameter (function with several parameters ar represented by a function of the first parameter that has a function (with the remaining parameters) as a result). [TFunc(generic,param,result)] represents a function where [param] is the type of the parameter, [result] is the type of the result, and [generic] is the list of universal types that are generalized (i.e., which can be different for each call of the function). [generic] will be non-empty only for functions that are bound by a let. For example, the type 'a -> 'b -> 'a will be represented by [TFunc([0;1],TUniv(0),TFunc([],TUniv(1),TUniv(0)))]*)
  | TList of int list * type_lang
      (** list type. [TList(generic,t)] represents a list of type [t] list. [generic] is the list of universal types that are generalized. For example, the empty list is of type [TList([0],TUniv 0)]*)
  | TUniv of int
      (** universal type, i.e. a type that can be replaced by any other type in a concrete execution. Represented as integer, but displayed as letter : [TUniv 0] is displayed as ['a], [TUniv 1] as ['b], etc.*)

val string_of_type_lang : type_lang -> string
(** Gets a string representation of a type. *)

(** Module representing annotations of expressions (i.e., AST nodes, which are the only one relevant here).
    Contains :
    - position in the source file (set by the parser, used for printing errors)
    - type of the expression (if already computed). *)
module Annotation : sig
  type t
  (** The type of annotations*)

  val create : Util.Position.t -> t
  (** [create pos] creates an annotation with the position set to [pos] and a type not set.*)

  val copy : t -> t

  val create_dummy : unit -> t
  (** Creates an annotation with a dummy position (useful for nodes created from other sources than the parser)*)

  val get_pos : t -> Util.Position.t
  (** [get_pos annotation] returns the position of [annotation].*)

  val get_type : t -> type_lang option
  (** [get_type annotation] returns [Some t] if annotation has [t] for type and [None] otherwise.*)

  val set_type : t -> type_lang -> unit
  (** [set_type annotation t] sets the type of [annotation] to [t].*)
end

(** Type representing the built-in function of the mini-ml language (both binary and unary). We also precise their type (universal types are generalised). *)
type built_in =
  | Add  (** Addition of integers : int -> int -> int *)
  | Sub  (** Substraction of integers : int -> int -> int *)
  | Mul  (** Multiplication of integers : int -> int -> int *)
  | Div  (** Quotient of integers : int -> int -> int *)
  | Mod  (** Modulus of integers : int -> int -> int *)
  | And  (** Boolean and : bool -> bool -> bool *)
  | Or  (** Boolean or : bool -> bool -> bool *)
  | Eq  (** Equality : 'a -> 'a -> bool *)
  | Neq  (** Difference : 'a -> 'a -> bool *)
  | Lt  (** Strictly less : 'a -> 'a -> bool *)
  | Gt  (** Strictly greater : 'a -> 'a -> bool *)
  | Leq  (** Less or equal : 'a -> 'a -> bool *)
  | Geq  (** Greater or equal : 'a -> 'a -> bool *)
  | Concat  (** String concatenation : string -> string -> string *)
  | Cat  (** List catenation : 'a -> 'a list -> 'a list *)
  | Append  (** List append : 'a list -> 'a list -> 'a list *)
  | UMin  (** Unary int minus : int -> int *)
  | Not  (** Boolean negation : bool -> bool *)
  | Head  (** List head : 'a list -> 'a *)
  | Tail  (** List tail : 'a list -> 'a list *)
  | Print  (** Print function : 'a -> unit *)

(** Type representing a mini-ml expression *)
type expr =
  | Cst_i of int * Annotation.t  (** Integer constant *)
  | Cst_b of bool * Annotation.t  (** Boolean constant *)
  | Cst_str of string * Annotation.t  (** String constant *)
  | Cst_func of built_in * Annotation.t  (** Built-in function constant *)
  | Nil of Annotation.t  (** Empty list constant *)
  | Unit of Annotation.t  (** Unit (empty action) constant *)
  | Var of string * Annotation.t  (** Variable *)
  | IfThenElse of expr * expr * expr * Annotation.t
      (** If then else expression *)
  | App of expr * expr * Annotation.t
      (** Function application (one argument) *)
  | Let of bool * string * expr * expr * Annotation.t
      (** Let in binding expression (the boolean is true if it is a recursive let) *)
  | Fun of string * expr * Annotation.t
      (** Function definition (one parameter) *)
  | Ignore of expr * expr * Annotation.t
      (** Expression sequencing (ignoring value of first expression) *)

type req = bool * string * expr
(** Type representing a top-level request (a let request) *)

type t = req list
(** Type of a mini-ml program, i.e., a list of {!req} *)

val copy_expr : expr -> expr

val get_expr_annotation : expr -> Annotation.t
(** [get_expr_annotation expr] returns the annotation contained in the expression. Useful for accessing them outside of a pattern-matching.*)

val pp_expr : bool -> bool -> Format.formatter -> expr -> unit
(** Pretty printer of an expression. [pp_expr extended_syntax print_types fmt expr] displays a text representation of [expr] on formatter [fmt]. If [extended_syntax] is true, will use extended OCaml-like syntax, otherwise, raw minimal syntax. If [print_types] is true, will annotate variables with their types if they are set.*)

val pp_req : bool -> bool -> Format.formatter -> req -> unit
(** Pretty printer of a request, see {!pp_expr} for roles of arguments. *)

val pp_program : bool -> bool -> Format.formatter -> req list -> unit
(** Pretty printer of a program, see {!pp_expr} for roles of arguments. *)

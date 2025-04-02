(** Module that represent environments that are useful for both interpreter and analyser.
    An environment is a dictionary from names (string) to some value.*)

type 'a t
(** The environment type*)

val new_environment : unit -> 'a t
(** Creates an empty environment.*)

val copy : 'a t -> 'a t
(** Creates a copy of the environment. Every cell of the copied environment shares its value with the original one, but isn't linked to it, so modifications to one won't be visible in the other.*)

val add : 'a t -> string -> 'a -> unit
(** [add environment name value] associates [value] to [name].*)

val remove : 'a t -> string -> unit
(** [remove environment name] removes the last value associated to [name]. Useful to handle correctly scopes of values.*)

val get : 'a t -> string -> 'a option
(** [get environment name] gets [Some v] if [v] is the value associated to [name], and returns [None] if it is not defined.*)

val pp_environment :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** [pp_environment printer fmt env] will print on [fmt] all values in the environment using the function [printer] to display the elements.*)

val map : 'a t -> ('a -> 'a) -> unit
(** [map env func] applies func to all values in the environment*)

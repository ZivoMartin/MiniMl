open Type_system
open Ast

exception Constraint_error of type_lang * type_lang
exception Typing_error of Util.Position.t * string

module Counter = struct
  type t = int ref

  let create () = ref 0

  let get_fresh counter =
    let res = !counter in
    counter := !counter + 1;
    res
end

let type_of_built_in (built_in : built_in) =
  match built_in with
  | Add -> TFunc ([], TInt, TFunc ([], TInt, TInt))
  | Sub -> TFunc ([], TInt, TFunc ([], TInt, TInt))
  | Mul -> TFunc ([], TInt, TFunc ([], TInt, TInt))
  | Div -> TFunc ([], TInt, TFunc ([], TInt, TInt))
  | Mod -> TFunc ([], TInt, TFunc ([], TInt, TInt))
  | And -> TFunc ([], TBool, TFunc ([], TBool, TBool))
  | Or -> TFunc ([], TBool, TFunc ([], TBool, TBool))
  | Eq -> TFunc ([], TInt, TFunc ([], TInt, TBool))
  | Neq -> TFunc ([], TInt, TFunc ([], TInt, TBool))
  | Lt -> TFunc ([], TInt, TFunc ([], TInt, TBool))
  | Gt -> TFunc ([], TInt, TFunc ([], TInt, TBool))
  | Leq -> TFunc ([], TInt, TFunc ([], TInt, TBool))
  | Geq -> TFunc ([], TInt, TFunc ([], TInt, TBool))
  | Concat -> TFunc ([], TString, TFunc ([], TString, TString))
  | Cat ->
      TFunc
        ( [],
          TList ([], TUniv 0),
          TFunc ([], TList ([], TUniv 0), TList ([], TUniv 0)) )
  | Append ->
      TFunc
        ( [],
          TList ([], TUniv 0),
          TFunc ([], TList ([], TUniv 0), TList ([], TUniv 0)) )
  | UMin -> TFunc ([], TInt, TInt)
  | Not -> TFunc ([], TBool, TBool)
  | Head -> TFunc ([], TList ([], TUniv 0), TUniv 0)
  | Tail -> TFunc ([], TList ([], TUniv 0), TUniv 0)
  | Print -> TFunc ([], TString, TUnit)

let rec solve_constraints (constraints : (type_lang * type_lang) list) :
    (int * type_lang) list =
  match constraints with
  (* à modifier après ça*)
  | [] -> failwith "solve_constraints not implemented"
  | ((TFunc _ as f), TBool) :: _ -> raise (Constraint_error (f, TBool))
  | _ :: t ->
      ignore substitute_constraint;
      solve_constraints t

let instantiate (counter : Counter.t) (type_lang : type_lang) : type_lang =
  (* à modifier*)
  let _ = Counter.get_fresh counter in
  match type_lang with _ -> type_lang

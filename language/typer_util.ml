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
  let rec substitute_constraint n1 t1 constraints =
    match constraints with 
    | [] -> []
    | (TFunc (l, targ, TUniv (n2)), t2) :: r when n1 = n2 ->
       substitute_constraint n1 t1 ((TFunc (l, targ, t1), t2) :: r)
    | (TFunc (l, TUniv (n2), tret), t2) :: r when n1 = n2 ->
       substitute_constraint n1 t1 ((TFunc (l, t1, tret), t2) :: r)

    | (t2, TFunc (l, targ, TUniv (n2))) :: r when n1 = n2 ->
       substitute_constraint n1 t1 ((t2, TFunc (l, targ, t1)) :: r)
    | (t2, TFunc (l, TUniv (n2), tret)) :: r when n1 = n2 ->
       substitute_constraint n1 t1 ((t2, TFunc (l, t1, tret)) :: r)

    | (TList (l, TUniv (n2)), t2) :: r when n1 = n2 ->
       substitute_constraint n1 t1 ((TList (l, t1), t2) :: r)
    | (t2, TList (l, TUniv (n2))) :: r when n1 = n2 ->
       substitute_constraint n1 t1 ((t2, TList (l, t1)) :: r)
    
    | (TUniv n2, t2) :: r  when n1 = n2 -> (t1, t2) :: substitute_constraint n1 t1 r
    | (t2, TUniv n2) :: r  when n1 = n2 -> (t2, t1) :: substitute_constraint n1 t1 r
    | c :: r -> c :: substitute_constraint n1 t1 r
  in
  match constraints with
  | [] -> []
  | (t1, t2) :: r when t1 = t2 -> solve_constraints r

  | (TUniv x1, TUniv x2) :: r when x1 <= x2 ->
   (x1, TUniv x2) :: solve_constraints (substitute_constraint x1 (TUniv x2) r)
  | (TUniv x2, TUniv x1) :: r ->
     (x1, TUniv x2) :: solve_constraints (substitute_constraint x1 (TUniv x2) r)

  | (TList (_, t1), TList (_, t2)) :: r -> solve_constraints ((t1, t2) :: r)
  | (TFunc (_, p1, r1), TFunc (_, p2, r2)) :: r -> solve_constraints ((p1, p2) :: (r1, r2) :: r)

  | (TUniv x, t) :: r | (t, TUniv x) :: r
    ->  (x, t) :: solve_constraints (substitute_constraint x t r)
  | (t1, t2) :: _ ->
     raise (Constraint_error (t1, t2))

let instantiate (counter : Counter.t) (type_lang : type_lang) : type_lang =
  (* à modifier*)
  let _ = Counter.get_fresh counter in
  match type_lang with _ -> type_lang

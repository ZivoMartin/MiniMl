open Type_system
open Ast

exception Constraint_error of type_lang * type_lang
exception Typing_error of Util.Position.t * string

module Counter = struct
  type t = int ref

  let create () = ref 0

  let get counter = !counter
  
  let get_fresh counter =
    let res = !counter in
    counter := !counter + 1;
    res
end

let type_of_built_in (built_in : built_in) =
  let u = TUniv 0 in
  match built_in with
  | Add -> TFunc ([], TInt, TFunc ([], TInt, TInt))
  | Sub -> TFunc ([], TInt, TFunc ([], TInt, TInt))
  | Mul -> TFunc ([], TInt, TFunc ([], TInt, TInt))
  | Div -> TFunc ([], TInt, TFunc ([], TInt, TInt))
  | Mod -> TFunc ([], TInt, TFunc ([], TInt, TInt))
  | And -> TFunc ([], TBool, TFunc ([], TBool, TBool))
  | Or -> TFunc ([], TBool, TFunc ([], TBool, TBool))
  | Eq -> TFunc ([0], u, TFunc ([], u, TBool))
  | Neq -> TFunc ([0], u, TFunc ([], u, TBool))
  | Lt -> TFunc ([0], u, TFunc ([], u, TBool))
  | Gt -> TFunc ([0], u, TFunc ([], u, TBool))
  | Leq -> TFunc ([0], u, TFunc ([], u, TBool))
  | Geq -> TFunc ([0], u, TFunc ([], u, TBool))
  | Concat -> TFunc ([], TString, TFunc ([], TString, TString))

  | Cat -> TFunc ([0], u, TFunc ([], TList ([], u), TList ([], u)))
  | Append -> TFunc ([0], TList ([], u), TFunc ([], TList ([], u), TList ([], u)))
  | Head -> TFunc ([0], TList ([], u), u)
  | Tail -> TFunc ([0], TList ([], u), TList ([], u))

  | UMin -> TFunc ([], TInt, TInt)
  | Not -> TFunc ([], TBool, TBool)
  | Print -> TFunc ([0], u, TUnit)


let solve_constraints (constraints : (type_lang * type_lang) list) :
    (int * type_lang) list =
  let rec solve_constraints_rec (constraints : (type_lang * type_lang) list) :
            (int * type_lang) list =
    let rec occurs x1 t = match t with
      | TFunc(_, t1, t2) -> occurs x1 t1 || occurs x1 t2
      | TUniv x2 -> x2 = x1
      | TList(_, t) -> occurs x1 t
      | _ -> false in

    
    let rec substitute_constraints n1 t1 constraints =
      match constraints with
      | [] -> []
      | (t2, t3) :: r ->
          (substitute_constraint n1 t1 (t2, t3)) :: substitute_constraints n1 t1 r
    in

    match constraints with
    | [] -> []

    | (t1, t2) :: r when t1 = t2 -> solve_constraints_rec r
    | (TUniv x1, TUniv x2) :: r when x1 < x2 ->
       (x1, TUniv x2) :: solve_constraints_rec (substitute_constraints x1 (TUniv x2) r)
    | (TUniv x2, TUniv x1) :: r ->
       (x2, TUniv x1) :: solve_constraints_rec (substitute_constraints x2 (TUniv x1) r)

    | (TList (_, t1), TList (_, t2)) :: r -> solve_constraints_rec ((t1, t2) :: r)
    | (TFunc (_, p1, r1), TFunc (_, p2, r2)) :: r ->
        solve_constraints_rec ((p1, p2) :: (r1, r2) :: r)

    | (TUniv x, t) :: r | (t, TUniv x) :: r ->
        if occurs x t then
          raise (Constraint_error (TUniv x, t))
        else (x, t) :: solve_constraints_rec (substitute_constraints x t r)

    | (t1, t2) :: _ -> raise (Constraint_error (t1, t2))
  in

  solve_constraints_rec constraints
  


let instantiate (counter : Counter.t) (type_lang : type_lang) : type_lang =
  let rec helper counter type_lang map =
    match type_lang with
    | TFunc (l, targ, tret) ->
       let l = map @ (List.map (fun x -> (x, Counter.get_fresh counter)) l) in
       TFunc ([], helper counter targ l, helper counter tret l)
    | TList (l, t) -> TList ([], helper counter t (map @ (List.map (fun x -> (x, Counter.get_fresh counter)) l)))
    | TUniv (x) as t -> (match List.find_opt (fun (original, _) -> original = x) map with
                   | Some ((_, fresh)) -> TUniv (fresh)
                   | None -> t)
    | _ -> type_lang
  in
  helper counter type_lang []

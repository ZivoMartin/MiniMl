open Ast

let get_type_generic = function TFunc (l, _, _) | TList (l, _) -> l | _ -> []

let get_free_type_var t =
  let rec aux = function
    | TFunc (generic, t1, t2) ->
        List.filter (fun t -> not (List.mem t generic)) (aux t1 @ aux t2)
    | TList (generic, t) ->
        List.filter (fun t -> not (List.mem t generic)) (aux t)
    | TUniv n -> [ n ]
    | _ -> []
  in
  List.sort_uniq compare (aux t)

(* [substitute_univ_in_type univ t target] will replace any occurrence of [TUniv univ] in [target] with [t] and return the result.

   For example [substitute_univ_in_type 0 TInt (TFunc([],TUniv 0,TFunc([] TUniv 1,TUniv 0))] will return [TFunc([],TInt,TFunc([] TUniv 1,TInt))]*)
let rec substitute_univ_in_type univ typ = function
  | TFunc (a, t1, t2) -> (
      match typ with
      | TUniv n ->
          TFunc
            ( List.map (fun s -> if s = univ then n else s) a,
              substitute_univ_in_type univ typ t1,
              substitute_univ_in_type univ typ t2 )
      | _ ->
          if List.mem univ a then TFunc (a, t1, t2)
          else
            TFunc
              ( a,
                substitute_univ_in_type univ typ t1,
                substitute_univ_in_type univ typ t2 ))
  | TList (a, t) -> (
      match typ with
      | TUniv n ->
          TList
            ( List.map (fun s -> if s = univ then n else s) a,
              substitute_univ_in_type univ typ t )
      | _ ->
          if List.mem univ a then TList (a, t)
          else TList (a, substitute_univ_in_type univ typ t))
  | TUniv n -> if n = univ then typ else TUniv n
  | t -> t

let apply_subst_in_type subst t =
  List.fold_left (fun acc (m, t) -> substitute_univ_in_type m t acc) t subst

let substitute_constraint univ typ (t1, t2) =
  (substitute_univ_in_type univ typ t1, substitute_univ_in_type univ typ t2)

(* [normalise_type floor target] replace any universal type with an index higher than [floor] in target such that no hole remains, and returns the list of replacement made. Useful for obtaining clean types after typing an expression.

   For example [normalise_type 4 (TFunc([], TUniv 0,TFunc([],TUniv 4,TUniv 42)))] will return ((TFunc([], TUniv 0,TFunc([],TUniv 4,TUniv 42))),[(4,4);(42,5)])*)
let normalise_type floor = function
  | TFunc (orig, arg, res) as t ->
      let l =
        List.mapi
          (fun i a -> (a, i + floor))
          (List.filter (fun v -> v >= floor) (get_free_type_var t))
      in
      let arg =
        List.fold_left
          (fun arg (a, i) -> substitute_univ_in_type a (TUniv i) arg)
          arg l
      in
      let res =
        List.fold_left
          (fun res (a, i) -> substitute_univ_in_type a (TUniv i) res)
          res l
      in
      ( TFunc
          ( List.sort_uniq compare
              (List.filter (fun a -> a < floor) orig @ List.map snd l),
            arg,
            res ),
        l )
  | TList (orig, t) ->
      let l =
        List.mapi
          (fun i a -> (a, i + floor))
          (List.filter (fun v -> v >= floor) (get_free_type_var t))
      in
      let t =
        List.fold_left
          (fun arg (a, i) -> substitute_univ_in_type a (TUniv i) arg)
          t l
      in
      ( TList
          ( List.sort_uniq compare
              (List.filter (fun a -> a < floor) orig @ List.map snd l),
            t ),
        l )
  | t -> (t, [])

let split_constraint_by_floor floor l =
  let outer =
    List.filter
      (fun (t1, t2) ->
        let free_var = get_free_type_var t1 @ get_free_type_var t2 in
        List.length (List.filter (fun n -> n >= floor) free_var) = 0
        && List.length free_var <> 0)
      l
  in
  let inner = List.filter (fun c -> not (List.mem c outer)) l in
  (inner, outer)

let rec one_type_substitution_in_expr (n, t) = function
  | Cst_i (_, ann)
  | Cst_b (_, ann)
  | Cst_str (_, ann)
  | Cst_func (_, ann)
  | Nil ann
  | Unit ann
  | Var (_, ann) ->
      Annotation.set_type ann
        (substitute_univ_in_type n t (Option.get (Annotation.get_type ann)))
  | Fun (_, e, ann) ->
      one_type_substitution_in_expr (n, t) e;
      Annotation.set_type ann
        (substitute_univ_in_type n t (Option.get (Annotation.get_type ann)))
  | App (e1, e2, ann) | Let (_, _, e1, e2, ann) | Ignore (e1, e2, ann) ->
      one_type_substitution_in_expr (n, t) e1;
      one_type_substitution_in_expr (n, t) e2;
      Annotation.set_type ann
        (substitute_univ_in_type n t (Option.get (Annotation.get_type ann)))
  | IfThenElse (e1, e2, e3, ann) ->
      one_type_substitution_in_expr (n, t) e1;
      one_type_substitution_in_expr (n, t) e2;
      one_type_substitution_in_expr (n, t) e3;
      Annotation.set_type ann
        (substitute_univ_in_type n t (Option.get (Annotation.get_type ann)))

let type_substitution_in_expr expr subst =
  List.iter (fun s -> one_type_substitution_in_expr s expr) subst

let generalize_type_expr floor expr =
  let t = Option.get (Annotation.get_type (get_expr_annotation expr)) in
  let t, subst = normalise_type floor t in
  type_substitution_in_expr expr (List.map (fun (a, i) -> (a, TUniv i)) subst);
  t

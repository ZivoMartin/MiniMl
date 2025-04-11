open Type_system
open Ast
open Typer_util

let rec type_expr (counter : Counter.t) (env : type_lang Util.Environment.t)
    (expr : expr) =
  match expr with
  | Cst_i (_, a) -> let _ = Annotation.set_type a TInt in (TInt, [])
  | Cst_b (_, a) -> let _ = Annotation.set_type a TBool in (TBool, [])
  | Cst_str (_, a) -> let _ = Annotation.set_type a TString in (TString, [])
  | Cst_func (b, a) ->
     let t = type_of_built_in b in
     let _ = Annotation.set_type a t in (t, [])
  | Nil (a) ->
     let t = TList ([], TUniv (Counter.get_fresh counter)) in
     let _ = Annotation.set_type a t in (t, [])
  | Unit (a) -> let _ = Annotation.set_type a TUnit in (TUnit, [])
  | Var (name, a) -> (
      match Util.Environment.get env name with
      | Some t ->
         (* let t_inst = instantiate counter t in *)
         let _ = Annotation.set_type a t in
         (t, [])
      | _ -> failwith ("Invalid var name: " ^ name)
    )
  | IfThenElse (cond, ifb, elseb, a) ->
     let ((t_cond, c0), (t1, c1), (t2, c2)) =
       (type_expr counter env cond,
        type_expr counter env ifb,
        type_expr counter env elseb)
     in
     let _ = Annotation.set_type a t1 in
     (t1, (t_cond, TBool) :: (t1, t2) :: c0 @ c1 @ c2)
  | Fun (name, e, a) ->
     let param_t = TUniv (Counter.get_fresh counter) in
     Util.Environment.add env name param_t;
     let (return_t, constraints) = type_expr counter env e in
     let t = TFunc ([], param_t, return_t) in
     Annotation.set_type a t;
     Util.Environment.remove env name;
     (t, constraints)
  | Ignore (e1, e2, a) ->
     let _ = type_expr counter env e1 in
     let (t, constraints) = type_expr counter env e2 in
     Annotation.set_type a t;
     (t, constraints)
  | App (f, arg, a) ->
     let (tf, c1) = type_expr counter env f in
     let (targ, c2) = type_expr counter env arg in
     let tf = instantiate counter tf in
     let tret = TUniv (Counter.get_fresh counter) in
     Annotation.set_type a tret;
     (tret, (tf, TFunc ([], targ, tret)) :: c1 @ c2)
  | Let (is_rec, name, e1, e2, a) ->
     let floor = Counter.get_fresh counter in

     let (_, constraints_var_building) =
       if is_rec then
         let fresh = TUniv (Counter.get_fresh counter) in
         Util.Environment.add env name fresh;
         let (t, constraints) = type_expr counter env e1 in
         Util.Environment.remove env name;
         (t, (fresh, t) :: constraints)
       else
         type_expr counter env e1
     in

     let (global_constraints, internal_constraints) = split_constraint_by_floor floor constraints_var_building in
     let sub = solve_constraints internal_constraints in
     type_substitution_in_expr e1 sub;
     let generalized = generalize_type_expr floor e1 in
     Util.Environment.add env name generalized;

     let (t, constraints_e2) = type_expr counter env e2 in
     let sub = sub @ (solve_constraints (global_constraints @ internal_constraints @ constraints_e2)) in
     type_substitution_in_expr e2 sub;
     let generalized = generalize_type_expr floor e2 in
     
     Annotation.set_type a generalized;
     Util.Environment.remove env name;
     (t,  [])
     

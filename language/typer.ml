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
         let _ = Annotation.set_type a t in
         (t, [])
      | _ -> failwith ("Invalid var name: " ^ name)
    )
  | IfThenElse (cond, ifb, elseb, a) ->
     let ((t1, c1), (t2, c2), (t_cond, c0)) =
        (type_expr counter env ifb,
        type_expr counter env elseb,
        type_expr counter env cond)
     in
     let _ = Annotation.set_type a t1 in
     let constraints = [(t_cond, TBool) ; (t1, t2)] in
     (t1, c0 @ c1 @ c2 @ constraints)
  | Fun (name, e, a) ->
     let param_t = TUniv (Counter.get_fresh counter) in
     Util.Environment.add env name param_t;
     let (return_t, constraints) = type_expr counter env e in
     let t = TFunc ([], param_t, return_t) in
     Annotation.set_type a t;
     Util.Environment.remove env name;
     (t, constraints)
  | Ignore (e1, e2, a) ->
     let (_, constraints1) = type_expr counter env e1 in
     let (t, constraints2) = type_expr counter env e2 in
     Annotation.set_type a t;
     (t, constraints1 @ constraints2)
  | App (f, arg, a) ->
     let tret = TUniv (Counter.get_fresh counter) in
     let (targ, carg) = type_expr counter env arg in
     let (tf, cf) = type_expr counter env f in
     let targ = instantiate counter targ in
     let tf = instantiate counter tf in

     Annotation.set_type a tret;
     (tret, (tf, TFunc ([], targ, tret)) :: carg @ cf)
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

     let sub_and_gen constraints e =
       let sub = solve_constraints constraints in
       type_substitution_in_expr e sub;
       generalize_type_expr floor e in

     let (internal_constraints, _) = split_constraint_by_floor floor constraints_var_building in
     let generalized = sub_and_gen internal_constraints e1 in
     Util.Environment.add env name generalized;

     let (_, constraints_e2) = type_expr counter env e2 in
     let generalized = sub_and_gen (constraints_var_building @ constraints_e2) e2 in
     Util.Environment.remove env name;

     Annotation.set_type a generalized;

     (generalized,  [])
     

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
         let t_inst = instantiate counter t in
         let _ = Annotation.set_type a t_inst in
         (t_inst, [])
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

     let (var_type, constraints_var_building) =
       if is_rec then
         let fresh = TUniv (Counter.get_fresh counter) in
         Util.Environment.add env name fresh;
         let (t, constraints) = type_expr counter env e1 in
         Util.Environment.remove env name;
         (t, (fresh, t) :: constraints)
       else
         type_expr counter env e1
     in

     let (_, internal_constraints) = split_constraint_by_floor floor constraints_var_building in
     let sub = solve_constraints internal_constraints in
     let var_type = apply_subst_in_type sub var_type in

     Annotation.set_type (get_expr_annotation e1) var_type;
     let generalised = generalize_type_expr floor e1 in
     Util.Environment.add env name generalised;

     let (t, constraints_e2) = type_expr counter env e2 in
     Annotation.set_type a t;
     Util.Environment.remove env name;
     
     try
       (t,  constraints_e2)
     with
     | Constraint_error (t1, t2) ->
        raise (Failure ("Unable to unify " ^ string_of_type_lang t1 ^ " and " ^ string_of_type_lang t2))

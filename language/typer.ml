open Type_system
open Ast
open Typer_util

let rec type_expr (counter : Counter.t) (env : type_lang Util.Environment.t)
    (expr : expr) =
  match expr with
  | Cst_i (_, a) -> let _ = Annotation.set_type a TInt in (TInt, [])
  | Cst_b (_, a) -> let _ = Annotation.set_type a TBool in (TBool, [])
  | Cst_str (_, a) -> let _ = Annotation.set_type a TString in (TString, [])
  | Cst_func (b, a) -> let t = type_of_built_in b in
                       let _ = Annotation.set_type a t in (t, [])
  | Nil (a) -> let u  = Counter.get_fresh counter in
               let t = TList ([0], TUniv 0) in
               let _ = Annotation.set_type a t in
               (t, [])
  | Unit (a) -> let _ = Annotation.set_type a TUnit in (TUnit, [])
  | Var (name, a) -> (
      match Util.Environment.get env name with
      | Some x -> let _ = Annotation.set_type a x in (x, [])
      | _ -> failwith ("Invalid var name: " ^ name))
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
     let t = TFunc ([],  param_t, return_t) in
     
     Annotation.set_type a t;
     Util.Environment.remove env name;
     (t, constraints)
  | Ignore (e1, e2, a) ->
     let (_, constraints1) = type_expr counter env e1 in
     let (t, constraints2) = type_expr counter env e2 in
     let _  = Annotation.set_type a t in
     (t, constraints1 @ constraints2)
  | App (f, arg, a) ->
     let (tf, cf) = type_expr counter env f in     
     let (targ, carg) = type_expr counter env arg in

     let gen_targ = TUniv (Counter.get_fresh counter) in
     let tret = TUniv (Counter.get_fresh counter) in
     
     let tf = instantiate counter tf in
     let targ = instantiate counter targ in
     
     Annotation.set_type a tret;
     let constraints = [(tf, TFunc ([], gen_targ, tret)) ; (gen_targ, targ)] in
     (tret, cf @ carg @ constraints)

  | Let (is_rec, name, e1, e2, a) ->
     let floor = Counter.get_fresh counter in
     let fresh = TUniv (Counter.get_fresh counter) in
     let (_, constraints_e1) =
       if is_rec then
         let _ = Util.Environment.add env name fresh in
         let (t, constraints) = type_expr counter env e1 in
         Util.Environment.remove env name;
         (t, (fresh, t) :: constraints)
       else
         type_expr counter env e1
     in

     let (internal_constraints, _) = split_constraint_by_floor floor constraints_e1 in
     let sub = solve_constraints internal_constraints in
     type_substitution_in_expr e1 sub;
     let generalized = generalize_type_expr floor e1 in
     Util.Environment.add env name generalized;

     let (_, constraints_e2) = type_expr counter env e2 in
     let sub = solve_constraints (constraints_e1 @ constraints_e2) in
     type_substitution_in_expr e2 sub;
     let t = match Annotation.get_type (get_expr_annotation e2) with
       | Some a -> a
       | None -> failwith "unreachable" in

     Util.Environment.remove env name;

     Annotation.set_type a t;

     (t,  [])

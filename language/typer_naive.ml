open Ast
open Typer_util

let rec type_expr (counter : Counter.t) (env : type_lang Util.Environment.t)
    (expr : expr) =
  match expr with
  | Cst_i (_, a) -> Annotation.set_type a TInt ; (TInt, [])
  | Cst_b (_, a) -> Annotation.set_type a TBool ; (TBool, [])
  | Cst_str (_, a) -> Annotation.set_type a TString ; (TString, [])
  | Cst_func (b, a) -> let t = type_of_built_in b in
                       let _ = Annotation.set_type a t in (t, [])
  | Nil (a) -> let u  = Counter.get_fresh counter in
               let t = TList ([u], TUniv u) in
               Annotation.set_type a t;
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
  | Let (is_rec, name, e1, e2, a) ->
     let (targ, constraints_var_building) =
       let fresh = TUniv (Counter.get_fresh counter) in
       if is_rec then
         let _ = Util.Environment.add env name fresh in
         let (t, constraints) = type_expr counter env e1 in
         (t, (fresh, t) :: constraints)
       else
         type_expr counter env e1 in
     Util.Environment.add env name targ;
     let (t, constraints) = type_expr counter env e2 in
     Annotation.set_type a t;
     Util.Environment.remove env name;
     (t, constraints_var_building @ constraints)
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
     
     let (tf, c1) = type_expr counter env f in
     let (targ, c2) = type_expr counter env arg in

     let gen_targ = TUniv (Counter.get_fresh counter) in
     let tret = TUniv (Counter.get_fresh counter) in

     Annotation.set_type a tret;
     let constraints = [(tf, TFunc ([], gen_targ, tret)) ; (gen_targ, targ)] in
     (tret, c1 @ c2 @ constraints)


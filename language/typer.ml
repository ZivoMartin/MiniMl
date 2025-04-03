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
  | Nil (a) -> let t = TList ([], TUniv (Counter.get_fresh counter)) in
               let _ = Annotation.set_type a t in (t, [])
  | Unit (a) -> let _ = Annotation.set_type a TUnit in (TUnit, [])
  | Var (name, a) -> (
      match Util.Environment.get env name with
      | Some x -> let _ = Annotation.set_type a x in (x, [])
      | _ -> failwith ("Invalid var name: " ^ name))
  | IfThenElse (cond, ifb, elseb, a) ->
     (match (type_expr counter env cond, type_expr counter env ifb, type_expr counter env elseb) with
                | ((TBool, c0), (t1, c1), (t2, c2)) when t1 = t2 -> let _ = Annotation.set_type a t1 in (t1, c0 @ c1 @ c2)
                | ((invalid_type, c0), (t1, c1), (t2, c2)) when t1 = t2 -> let _ = Annotation.set_type a t1 in (t1, (TBool, invalid_type)::(c0 @ c1 @ c2))
                | ((TBool, c0), (t1, c1), (t2, c2))  -> let _ = Annotation.set_type a t1  in (t1, (t1, t2)::(c0 @ c1 @ c2))
                | ((invalid_type, c0), (t1, c1), (t2, c2)) -> let _ = Annotation.set_type a t1 in (t1, (t1, t2)::(TBool, invalid_type)::(c0 @ c1 @ c2)))
  | Let (is_rec, name, e1, e2, a) ->
     let constraints_var_building =
       if is_rec then
         let fresh = TUniv (Counter.get_fresh counter) in
         Util.Environment.add env name fresh;
         let (t, constraints) = type_expr counter env e1 in
         (fresh, t)::constraints
       else
         let (t, constraints) = type_expr counter env e1 in
         let _ = Util.Environment.add env name t in
         constraints in
     let (t, constraints) = type_expr counter env e2 in
     let _  = Annotation.set_type a t in
     let _ = Util.Environment.remove env name in (t, constraints_var_building @ constraints)
  | Fun (name, e, a) ->
     let param_t = TUniv (Counter.get_fresh counter) in
     let _ = Util.Environment.add env name param_t in
     let (return_t, constraints) = type_expr counter env e in
     let t = TFunc ([],  param_t, return_t) in
     let _  = Annotation.set_type a t in
     let _ = Util.Environment.remove env name in (t, constraints)
  | Ignore (e1, e2, a) ->
     let _ = type_expr counter env e1 in
     let (t, constraints) = type_expr counter env e2 in
     let _  = Annotation.set_type a t in (t, constraints)
  | App (f, arg, a) ->
     let (tf, c1) = type_expr counter env f in
     let (targ, c2) = type_expr counter env arg in
     let tret = TUniv (Counter.get_fresh counter) in
     Annotation.set_type a tret;
     (tret, (tf, TFunc ([], targ, tret)) :: c1 @ c2)

(* let rec type_expr (counter : Counter.t) (env : type_lang Util.Environment.t) *)
(*     (expr : expr) = *)
(*   (\* la suite est à modifier -- c’est juste là pour ne pas avoir de warning tant que vous ne travaillez pas dessus.*\) *)
(*   ignore generalize_type_expr; *)
(*   match expr with *)
(*   | App (e, _, _) -> type_expr counter env e *)
(*   | _ -> failwith "full typer not implemented" *)

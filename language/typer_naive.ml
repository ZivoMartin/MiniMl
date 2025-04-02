open Ast
open Typer_util

let rec type_expr (counter : Counter.t) (env : type_lang Util.Environment.t)
    (expr : expr) =
  match expr with
  | App (e, _, _) -> type_expr counter env e
  | Cst_i _ -> TInt
  | Cst_b _ -> TBool
  | Cst_str _ -> TString
  | Cst_func (b, _) -> type_of_built_in b
  | Nil _ -> TList ([], TUniv 0)
  | Unit _ -> TUnit
  | Var (name, _) -> (
      match Util.Environment.get env name with
      | Some x -> x
      | _ -> failwith ("Invalid var name: " ^ name))
  | IfThenElse (cond, ifb, elseb, _) -> type_expr counter env ifb
  | Let _ -> failwith "err"
  | Fun (name, e, _) -> failwith "err"
  | Ignore (e1, e2, _) -> failwith "err"

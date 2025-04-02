open Ast

type value =
  | Int of int
  | Bool of bool
  | String of string
  | Unit
  | Func of (value -> value)
  | List of value list

let rec pp_value fmt = function
  | Int i -> Format.fprintf fmt "%d" i
  | Bool b -> Format.fprintf fmt "%b" b
  | String s -> Format.fprintf fmt "%s" s
  | Unit -> Format.fprintf fmt "()"
  | Func _ -> Format.fprintf fmt "<fun>"
  | List l ->
      Format.fprintf fmt "[";
      List.iteri
        (fun i e ->
          Format.fprintf fmt "%s%a" (if i = 0 then "" else ";") pp_value e)
        l;
      Format.fprintf fmt "]"

let func_of_built_in = function
  | Add ->
      Func
        (function
        | Int v1 ->
            Func
              (function
              | Int v2 -> Int (v1 + v2)
              | _ -> failwith "second argument of Add is not an integer")
        | _ -> failwith "first argument of Add is not an integer")
  | Sub ->
      Func
        (function
        | Int v1 ->
            Func
              (function
              | Int v2 -> Int (v1 - v2)
              | _ -> failwith "second argument of Sub is not an integer")
        | _ -> failwith "first argument of Sub is not an integer")
  | Mul ->
      Func
        (function
        | Int v1 ->
            Func
              (function
              | Int v2 -> Int (v1 * v2)
              | _ -> failwith "second argument of Mul is not an integer")
        | _ -> failwith "first argument of Mul is not an integer")
  | Div ->
      Func
        (function
        | Int v1 ->
            Func
              (function
              | Int v2 -> Int (v1 / v2)
              | _ -> failwith "second argument of Div is not an integer")
        | _ -> failwith "first argument of Div is not an integer")
  | Mod ->
      Func
        (function
        | Int v1 ->
            Func
              (function
              | Int v2 -> Int (v1 mod v2)
              | _ -> failwith "second argument of Mod is not an integer")
        | _ -> failwith "first argument of Mod is not an integer")
  | And ->
      Func
        (function
        | Bool v1 ->
            Func
              (function
              | Bool v2 -> Bool (v1 && v2)
              | _ -> failwith "second argument of And is not a bool")
        | _ -> failwith "first argument of And is not a bool")
  | Or ->
      Func
        (function
        | Bool v1 ->
            Func
              (function
              | Bool v2 -> Bool (v1 || v2)
              | _ -> failwith "second argument of Or is not a bool")
        | _ -> failwith "first argument of Or is not a bool")
  | Eq -> Func (fun v1 -> Func (fun v2 -> Bool (v1 = v2)))
  | Neq -> Func (fun v1 -> Func (fun v2 -> Bool (v1 <> v2)))
  | Lt -> Func (fun v1 -> Func (fun v2 -> Bool (v1 < v2)))
  | Gt -> Func (fun v1 -> Func (fun v2 -> Bool (v1 > v2)))
  | Leq -> Func (fun v1 -> Func (fun v2 -> Bool (v1 <= v2)))
  | Geq -> Func (fun v1 -> Func (fun v2 -> Bool (v1 >= v2)))
  | Concat ->
      Func
        (function
        | String v1 ->
            Func
              (function
              | String v2 -> String (v1 ^ v2)
              | _ -> failwith "second argument of Concat is not a string")
        | _ -> failwith "first argument of Concat is not a string")
  | Cat ->
      Func
        (fun v1 ->
          Func
            (function
            | List v2 -> List (v1 :: v2)
            | _ -> failwith "second argument of Cat is not a list"))
  | Append ->
      Func
        (function
        | List v1 ->
            Func
              (function
              | List v2 -> List (v1 @ v2)
              | _ -> failwith "second argument of Append is not a list")
        | _ -> failwith "first argument of Append is not a list")
  | UMin ->
      Func
        (function
        | Int v -> Int (-v) | _ -> failwith "argument of UMin is not an integer")
  | Not ->
      Func
        (function
        | Bool v -> Bool (not v) | _ -> failwith "argument of Not is not a bool")
  | Head ->
      Func
        (function
        | List v -> List.hd v | _ -> failwith "argument of Head is not a list")
  | Tail ->
      Func
        (function
        | List v -> List (List.tl v)
        | _ -> failwith "argument of Tail is not a list")
  | Print ->
      Func
        (fun v ->
          (match v with
          | String s -> Format.printf "%s" (Scanf.unescaped s)
          | _ -> Format.printf "%a" pp_value v);
          Unit)

let rec interpret_expression env = function
  | Cst_i (i, _) -> Int i
  | Cst_b (b, _) -> Bool b
  | Cst_str (s, _) -> String s
  | Cst_func (b, _) -> func_of_built_in b
  | Nil _ -> List []
  | Unit _ -> Unit
  | Var (x, _) -> (
      try Option.get (Util.Environment.get env x)
      with _ -> failwith (Format.sprintf "error with var %s" x))
  | IfThenElse (test, th, el, _) -> (
      match interpret_expression env test with
      | Bool true -> interpret_expression env th
      | Bool false -> interpret_expression env el
      | _ -> failwith "non boolean value")
  | App (e1, e2, _) -> (
      let v = interpret_expression env e2 in
      match interpret_expression env e1 with
      | Func f -> f v
      | _ -> failwith "non functional application")
  | Let (_, x, e1, e2, _) ->
      let v = interpret_expression env e1 in
      Util.Environment.add env x v;
      let res = interpret_expression env e2 in
      Util.Environment.remove env x;
      res
  | Fun (n, e, _) ->
      Func
        (fun v ->
          let env2 = Util.Environment.copy env in
          Util.Environment.add env2 n v;
          interpret_expression env2 e)
  | Ignore (e1, e2, _) ->
      let _ = interpret_expression env e1 in
      interpret_expression env e2

let interpret_program env prg =
  List.iter
    (fun (_, name, expr) ->
      let v = interpret_expression env expr in
      Util.Environment.add env name v;
      Format.printf "Computed %s = %a@," name pp_value v)
    prg

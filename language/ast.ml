type type_lang =
  | TInt
  | TBool
  | TString
  | TUnit
  | TFunc of int list * type_lang * type_lang
  | TList of int list * type_lang
  | TUniv of int

let rec string_of_type_lang = function
  | TInt -> "int"
  | TBool -> "bool"
  | TString -> "string"
  | TUnit -> "unit"
  | TFunc (l, t1, t2) ->
      (if l = [] then ""
       else
         List.fold_left
           (fun acc i ->
             Format.sprintf "%s,'%c" acc (char_of_int (i + int_of_char 'a')))
           "(" l
         ^ ")")
      ^ Format.sprintf "%s -> %s"
          (let s = string_of_type_lang t1 in
           match t1 with TFunc _ -> Format.sprintf "(%s)" s | _ -> s)
          (string_of_type_lang t2)
  | TList (l, t) ->
      (if l = [] then ""
       else
         List.fold_left
           (fun acc i ->
             Format.sprintf "%s,'%c" acc (char_of_int (i + int_of_char 'a')))
           "(" l
         ^ ")")
      ^ Format.sprintf "%s list" (string_of_type_lang t)
  | TUniv n ->
      if n < 26 then Format.sprintf "'%c" (char_of_int (n + int_of_char 'a'))
      else Format.sprintf "'_%d" (n - 26)

module Annotation = struct
  type t = {
    mutable position : Util.Position.t;
    mutable typ : type_lang option;
  }

  let create pos = { position = pos; typ = None }
  let copy annotation = { position = annotation.position; typ = annotation.typ }
  let create_dummy () = create (Lexing.dummy_pos, Lexing.dummy_pos)
  let get_pos annotation = annotation.position
  let get_type annotation = annotation.typ
  let set_type annotation typ = annotation.typ <- Some typ
end

type built_in =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Eq
  | Neq
  | Lt
  | Gt
  | Leq
  | Geq
  | Concat
  | Cat
  | Append
  | UMin
  | Not
  | Head
  | Tail
  | Print

type expr =
  | Cst_i of int * Annotation.t
  | Cst_b of bool * Annotation.t
  | Cst_str of string * Annotation.t
  | Cst_func of built_in * Annotation.t
  | Nil of Annotation.t
  | Unit of Annotation.t
  | Var of string * Annotation.t
  | IfThenElse of expr * expr * expr * Annotation.t
  | App of expr * expr * Annotation.t
  | Let of bool * string * expr * expr * Annotation.t
  | Fun of string * expr * Annotation.t
  | Ignore of expr * expr * Annotation.t

type req = bool * string * expr
type t = req list

let rec copy_expr = function
  | Cst_i (i, ann) -> Cst_i (i, Annotation.copy ann)
  | Cst_b (b, ann) -> Cst_b (b, Annotation.copy ann)
  | Cst_str (s, ann) -> Cst_str (s, Annotation.copy ann)
  | Cst_func (f, ann) -> Cst_func (f, Annotation.copy ann)
  | Nil ann -> Nil (Annotation.copy ann)
  | Unit ann -> Unit (Annotation.copy ann)
  | Var (x, ann) -> Var (x, Annotation.copy ann)
  | IfThenElse (test, th, el, ann) ->
      IfThenElse
        (copy_expr test, copy_expr th, copy_expr el, Annotation.copy ann)
  | App (f, arg, ann) -> App (copy_expr f, copy_expr arg, Annotation.copy ann)
  | Let (r, name, e1, e2, ann) ->
      Let (r, name, copy_expr e1, copy_expr e2, Annotation.copy ann)
  | Fun (name, body, ann) -> Fun (name, copy_expr body, Annotation.copy ann)
  | Ignore (e1, e2, ann) ->
      Ignore (copy_expr e1, copy_expr e2, Annotation.copy ann)

let get_expr_annotation = function
  | Cst_i (_, ann)
  | Cst_b (_, ann)
  | Cst_str (_, ann)
  | Cst_func (_, ann)
  | Nil ann
  | Unit ann
  | Var (_, ann)
  | IfThenElse (_, _, _, ann)
  | App (_, _, ann)
  | Let (_, _, _, _, ann)
  | Fun (_, _, ann)
  | Ignore (_, _, ann) ->
      ann

let string_of_built_in = function
  | Add -> "( + )"
  | Sub -> "( - )"
  | Mul -> "( * )"
  | Div -> "( / )"
  | Mod -> "( % )"
  | And -> "( && )"
  | Or -> "( || )"
  | Eq -> "( = )"
  | Neq -> "( ≠ )"
  | Lt -> "( < )"
  | Gt -> "( > )"
  | Leq -> "( ≤ )"
  | Geq -> "( ≥ )"
  | Concat -> "( ^ )"
  | Cat -> "( :: )"
  | Append -> "( @ )"
  | UMin -> "-"
  | Not -> "¬"
  | Head -> "hd"
  | Tail -> "tl"
  | Print -> "print"

let infix_string_of_built_in = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | And -> "&&"
  | Or -> "||"
  | Eq -> "="
  | Neq -> "≠"
  | Lt -> "<"
  | Gt -> ">"
  | Leq -> "≤"
  | Geq -> "≥"
  | Concat -> "^"
  | Cat -> "::"
  | Append -> "@"
  | _ -> assert false

let is_binary_built_in = function
  | UMin | Not | Head | Tail | Print -> false
  | _ -> true

let get_arg_list_of_fun =
  let rec aux acc_var acc_typ = function
    | Fun (n, e, ann) ->
        aux (n :: acc_var)
          ((match Annotation.get_type ann with
           | Some (TFunc (_, t, _)) -> t
           | _ -> TUnit)
          :: acc_typ)
          e
    | e -> (List.rev acc_var, List.rev acc_typ, e)
  in
  aux [] []

let rec get_result_type prof = function
  | TFunc (_, _, t_res) as t ->
      if prof <= 1 then t else get_result_type (prof - 1) t_res
  | t -> t

let rec pp_expr extended_syntax print_types fmt = function
  | Cst_i (i, _) -> Format.fprintf fmt "%d" i
  | Cst_b (b, _) -> Format.fprintf fmt "%b" b
  | Cst_str (s, _) -> Format.fprintf fmt "\"%s\"" s
  | Cst_func (b, _) -> Format.fprintf fmt "%s" (string_of_built_in b)
  | Nil _ -> Format.fprintf fmt "[]"
  | Unit _ -> Format.fprintf fmt "()"
  | Var (x, ann) -> (
      try
        assert print_types;
        Format.fprintf fmt "(%s : %s)" x
          (string_of_type_lang (Option.get (Annotation.get_type ann)))
      with _ -> Format.fprintf fmt "%s" x)
  | IfThenElse (test, th, el, _) ->
      Format.fprintf fmt "@[<v 0>@[<v 2>if %a then@,%a@]@,@[<v 2>else@,%a@]@]"
        (pp_expr extended_syntax print_types)
        test
        (pp_expr extended_syntax print_types)
        th
        (pp_expr extended_syntax print_types)
        el
  | App (App (Cst_func (b, _), e1, _), e2, _)
    when extended_syntax && is_binary_built_in b ->
      Format.fprintf fmt
        (match e1 with
        | Cst_i _ | Cst_b _ | Cst_str _ | Nil _ | Var _ -> "%a"
        | _ -> "(%a)")
        (pp_expr extended_syntax print_types)
        e1;
      Format.fprintf fmt " %s " (infix_string_of_built_in b);
      Format.fprintf fmt
        (match e2 with
        | Cst_i _ | Cst_b _ | Cst_str _ | Nil _ | Var _ -> "%a"
        | _ -> "(%a)")
        (pp_expr extended_syntax print_types)
        e2
  | App (e1, e2, _) ->
      Format.fprintf fmt
        (match e1 with
        | Cst_i _ | Cst_b _ | Cst_str _ | Cst_func _ | Nil _ | Var _ | App _ ->
            "%a "
        | _ -> "(%a) ")
        (pp_expr extended_syntax print_types)
        e1;
      Format.fprintf fmt
        (match e2 with
        | Cst_i _ | Cst_b _ | Cst_str _ | Nil _ | Var _ -> "%a"
        | _ -> "(%a)")
        (pp_expr extended_syntax print_types)
        e2
  | Let (r, x, e1, e2, _) ->
      if extended_syntax then (
        let l_var, l_typ, e1 = get_arg_list_of_fun e1 in
        (try
           assert print_types;
           Format.fprintf fmt "@[<v 2>let%s %s : %s"
             (if r then " rec" else "")
             (List.fold_left2
                (fun acc v t ->
                  Format.sprintf "%s (%s : %s)" acc v (string_of_type_lang t))
                x l_var l_typ)
             (string_of_type_lang
                (get_result_type (List.length l_var)
                   (Option.get (Annotation.get_type (get_expr_annotation e1)))))
         with _ ->
           Format.fprintf fmt "@[<v 2>let%s %s"
             (if r then " rec" else "")
             (List.fold_left
                (fun acc v -> Format.sprintf "%s %s" acc v)
                x l_var));
        Format.fprintf fmt " =@,%a@]@,in@,%a"
          (pp_expr extended_syntax print_types)
          e1
          (pp_expr extended_syntax print_types)
          e2)
      else (
        (try
           assert print_types;
           Format.fprintf fmt "@[<v 2>let%s (%s : %s)"
             (if r then " rec" else "")
             x
             (string_of_type_lang
                (Option.get (Annotation.get_type (get_expr_annotation e1))))
         with _ ->
           Format.fprintf fmt "@[<v 2>let%s %s" (if r then " rec" else "") x);
        Format.fprintf fmt " =@,%a@]@,in@, %a"
          (pp_expr extended_syntax print_types)
          e1
          (pp_expr extended_syntax print_types)
          e2)
  | Fun (n, e, ann) ->
      Format.fprintf fmt "@[<v 0>fun %s %s->@,%a@]" n
        (try
           assert print_types;
           Format.sprintf ": %s"
             (string_of_type_lang
                (match Option.get (Annotation.get_type ann) with
                | TFunc (_, t, _) -> t
                | _ -> failwith "catched"))
         with _ -> "")
        (pp_expr extended_syntax print_types)
        e
  | Ignore (e1, e2, _) ->
      Format.fprintf fmt "%a;@, %a"
        (pp_expr extended_syntax print_types)
        e1
        (pp_expr extended_syntax print_types)
        e2

let pp_req extended_syntax print_types fmt (r, name, expr) =
  if extended_syntax then (
    let l_var, l_typ, expr = get_arg_list_of_fun expr in
    (try
       assert print_types;
       Format.fprintf fmt "@[<v 2>let%s %s : %s"
         (if r then " rec" else "")
         (List.fold_left2
            (fun acc v t ->
              Format.sprintf "%s (%s : %s)" acc v (string_of_type_lang t))
            name l_var l_typ)
         (string_of_type_lang
            (get_result_type (List.length l_var)
               (Option.get (Annotation.get_type (get_expr_annotation expr)))))
     with _ ->
       Format.fprintf fmt "@[<v 2>let%s %s"
         (if r then " rec" else "")
         (List.fold_left (fun acc v -> Format.sprintf "%s %s" acc v) name l_var));
    Format.fprintf fmt " =@,%a@]@," (pp_expr extended_syntax print_types) expr)
  else (
    (try
       assert print_types;
       Format.fprintf fmt "@[<v 2>let%s (%s : %s)"
         (if r then " rec" else "")
         name
         (string_of_type_lang
            (Option.get (Annotation.get_type (get_expr_annotation expr))))
     with _ ->
       Format.fprintf fmt "@[<v 2>let%s %s" (if r then " rec" else "") name);
    Format.fprintf fmt " =@,%a@]@," (pp_expr extended_syntax print_types) expr)

let pp_program extended_syntax print_types fmt prg =
  Format.fprintf fmt "@[<v 0>";
  List.iter
    (fun req ->
      Format.fprintf fmt "%a" (pp_req extended_syntax print_types) req)
    prg;
  Format.fprintf fmt "@]"

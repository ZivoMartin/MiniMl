open Language

module Command_line = struct
  type parser = Calc | Simple | Extended | Sandbox
  (* ajouter une autre valeur ici si vous voulez rajouter un parseur -- et dans les deux fonctions suivantes. *)

  let parser_of_string = function
    | "calc" -> Calc
    | "raw" -> Simple
    | "extended" -> Extended
    | "sandbox" -> Sandbox
    | _ -> failwith "not a valid parser"

  let string_of_parser = function
    | Calc -> "calc"
    | Simple -> "raw"
    | Extended -> "extended"
    | Sandbox -> "sandbox"

  let parse_from_file = ref true
  let bypass_error = ref false
  let parser = ref Extended
  let print_extended_syntax = ref true
  let print_annotated_program = ref true
  let use_typer = ref true
  let naive_typer = ref true
  let full_typer = ref true
  let execute_program = ref true

  let arg_spec_list =
    [
      ( "-from-stdin",
        Arg.Clear parse_from_file,
        " if present, the argument is a string to parse, otherwise it is the \
         name of the file containing the program." );
      ( "-bypass-error",
        Arg.Set bypass_error,
        " if present, executes incorrect programs -- probably leading to a \
         crash." );
      ( "-parser",
        Arg.Symbol
          ([ "calc"; "raw"; "extended" ], fun s -> parser := parser_of_string s),
        " determines which parser is used for parsing the input. (default: "
        ^ string_of_parser !parser ^ ")" );
      ( "-print-raw-syntax",
        Arg.Clear print_extended_syntax,
        " if present, displays program using the raw syntax instead of the \
         extended one." );
      ( "-skip-typer",
        Arg.Clear use_typer,
        " if present, skips the typing phase." );
      ( "-no-annotation-printing",
        Arg.Clear print_annotated_program,
        " if present, skips the displaying of the program annotated with types \
         after the typing phase" );
      ( "-no-simple-typer",
        Arg.Clear naive_typer,
        " if present, doesn't use the weak polymorphic typer (in typer_naive)"
      );
      ( "-no-full-typer",
        Arg.Clear full_typer,
        " if present, doesn't use the full polymorphic type (in typer)" );
      ( "-no-execution",
        Arg.Clear execute_program,
        " if present, skips the execution of the program." );
    ]

  let usage_msg =
    "Usage: " ^ Sys.argv.(0)
    ^ " [argument]\n\
       Parses the argument with the parser, runs the semantic analysis phase \
       on it, and if no error was detected runs the interpreter on the program \
       parsed.\n\
       It also displays the parsed program before and after semantic analysis.\n\
       [argument] is either the string to parse or the filename to be analysed \
       (if [-from-stdin] is present)\n"

  let parse () =
    let res = ref None in
    let args = ref [] in
    Arg.parse (Arg.align arg_spec_list)
      (fun a ->
        match !res with None -> res := Some a | Some _ -> args := a :: !args)
      usage_msg;
    match !res with
    | None ->
        Arg.usage arg_spec_list usage_msg;
        exit 0
    | Some s -> (s, List.rev !args)
end

let input, args = Command_line.parse ()

let parser =
  match !Command_line.parser with
  | Extended -> Parser.main
  | Simple -> Parser_simple.main
  | Calc -> Parser_calc.main
  | Sandbox -> Parser_sandbox.main
(* rajouter un autre parseur ici si nécessaire. *)

let text, lexbuf =
  match !Command_line.parse_from_file with
  | true -> MenhirLib.LexerUtil.read input
  | false -> (input, Lexing.from_string input)

let program = parser Lexer.token lexbuf

let () =
  Format.printf "@[<v 0>Parsed program:@,%a@,"
    Ast.(pp_program !Command_line.print_extended_syntax false)
    program

let program_copy =
  List.map (fun (r, name, expr) -> (r, name, Ast.copy_expr expr)) program

let typing_naive_successfull =
  if !Command_line.use_typer then
    if !Command_line.naive_typer then (
      let error_during_typing = ref false in
      Format.printf "@[<v 2>--Weak typing--@,";
      let counter = Typer_util.Counter.create () in
      let env = Util.Environment.new_environment () in
      let typed_program =
        List.map
          (fun (recursive, name, expr) ->
            let num = Typer_util.Counter.get_fresh counter in
            if recursive then Util.Environment.add env name (Ast.TUniv num);
            let typ, constraints =
              try Typer_naive.type_expr counter env expr with
              | Typer_util.Typing_error (pos, message) ->
                  error_during_typing := true;
                  Format.printf "@[<v 2>Typing error for %s in:@,%a@,%s@]@,"
                    name Util.Position.pp_position (text, pos) message;
                  let a = Typer_util.Counter.get_fresh counter in
                  (Ast.TUniv a, [])
              | Failure s ->
                  error_during_typing := true;
                  Format.printf "Error wile typing %s:@, %s@," name s;
                  let a = Typer_util.Counter.get_fresh counter in
                  (Ast.TUniv a, [])
            in
            let constraints =
              if recursive then (Ast.TUniv num, typ) :: constraints
              else constraints
            in
            Util.Environment.add env name typ;
            (name, typ, constraints, expr))
          program_copy
      in
      let () =
        List.iter
          (fun (name, typ, constraints, _) ->
            Format.printf "%s : %s @,  constraints : [%a]@," name
              (Ast.string_of_type_lang typ)
              (fun fmt constraints ->
                List.iter
                  (fun (t1, t2) ->
                    Format.fprintf fmt ",%s = %s"
                      (Ast.string_of_type_lang t1)
                      (Ast.string_of_type_lang t2))
                  constraints)
              constraints)
          typed_program
      in
      let res =
        (Format.printf "---Solving constraints:@,";
         try
           let _ =
             List.fold_left
               (fun subst (name, typ, constraints, expr) ->
                 let constraints =
                   List.map
                     (fun (t1, t2) ->
                       ( Type_system.apply_subst_in_type subst t1,
                         Type_system.apply_subst_in_type subst t2 ))
                     constraints
                 in
                 let s1 = Typer_util.solve_constraints constraints in
                 let subst = subst @ s1 in
                 Type_system.type_substitution_in_expr expr subst;
                 let typ = Type_system.apply_subst_in_type subst typ in
                 Format.printf "%s : %s@," name (Ast.string_of_type_lang typ);
                 subst)
               [] typed_program
           in
           true
         with
         | Typer_util.Constraint_error (t1, t2) ->
             Format.printf "Unable to unify %s and %s@,"
               (Ast.string_of_type_lang t1)
               (Ast.string_of_type_lang t2);
             false
         | _ ->
             Format.printf
               "Received failure, have you implemented everything?@,";
             false)
        && not !error_during_typing
      in
      if !Command_line.print_annotated_program then
        Format.printf "Annotated program:@,%a@,"
          Ast.(pp_program !Command_line.print_extended_syntax true)
          program_copy;
      Format.printf "@]@,";
      res)
    else false
  else true

let typing_successfull =
  if !Command_line.use_typer then
    if !Command_line.full_typer then (
      Format.printf "@[<v 3>---Full typing---@,";
      let res =
        try
          let types =
            let env = Util.Environment.new_environment () in
            List.map
              (fun (recursive, name, expr) ->
                let counter = Typer_util.Counter.create () in
                let type_expr, constraints =
                  Typer.type_expr counter env
                    (Let
                       ( recursive,
                         name,
                         expr,
                         Var (name, Ast.Annotation.create_dummy ()),
                         Ast.Annotation.create_dummy () ))
                in
                Util.Environment.add env name type_expr;
                (name, type_expr, constraints))
              program
          in
          Format.printf "Types computed:@,";
          List.iter
            (fun (n, t, c) ->
              Format.printf "%s : %s@," n (Ast.string_of_type_lang t);
              if c <> [] then
                Format.printf
                  "Warning, the following constraints are left unsolved: [%a]@,"
                  (fun fmt constraints ->
                    List.iter
                      (fun (t1, t2) ->
                        Format.fprintf fmt ",%s = %s"
                          (Ast.string_of_type_lang t1)
                          (Ast.string_of_type_lang t2))
                      constraints)
                  c)
            types;
          Format.printf "@,";
          true
        with
        | Failure s ->
            Format.printf
              "The program didn't type correctly@,Error returned:@,%s@,@," s;
            false
        | Typer_util.Typing_error (pos, message) ->
            Format.printf "@[<v 2>Typing error in:@,%a@,%s@]@,"
              Util.Position.pp_position (text, pos) message;
            false
      in
      if !Command_line.print_annotated_program then
        Format.printf "Annotated program:@,%a@,"
          Ast.(pp_program !Command_line.print_extended_syntax true)
          program;
      Format.printf "@]@,";
      res)
    else false
  else true

let typing_successfull = typing_successfull || typing_naive_successfull

let () =
  if
    !Command_line.execute_program
    && (!Command_line.bypass_error || typing_successfull)
  then (
    Format.printf "Execution of the program:@,";
    try
      let env = Util.Environment.new_environment () in
      Interpreter.interpret_program env program
    with Failure s ->
      Format.printf
        "The execution of the program caused the following error:@,%s@," s)

let () = Format.printf "@]"

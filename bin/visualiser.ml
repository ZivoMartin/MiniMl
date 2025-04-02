module Command_line = struct
  let parse_from_file = ref true

  type parser = Calc | Simple | Extended | Sandbox
  (*rajouter un autre parseur ici si nécessaire (et dans les deux fonctions suivantes).*)

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

  let parser = ref Extended

  let arg_spec_list =
    [
      ( "-from-stdin",
        Arg.Clear parse_from_file,
        " if present, the argument is a string to parse, otherwise it is the \
         name of the file containing the program." );
      ( "-parser",
        Arg.Symbol
          ([ "calc"; "raw"; "extended" ], fun s -> parser := parser_of_string s),
        " determines which parser is used for parsing the input. (default: "
        ^ string_of_parser !parser ^ ")" );
    ]

  let usage_msg =
    "Usage: " ^ Sys.argv.(0)
    ^ " [argument]\n\
       Parses the argument with the parser and displays the execution of the \
       parser step by step.\n\
       [argument] is either the string to parse or the filename to be analysed \
       (if [-from-stdin] is present)\n"

  let parse () =
    let res = ref None in
    Arg.parse (Arg.align arg_spec_list)
      (fun a ->
        match !res with
        | None -> res := Some a
        | Some _ -> raise (Arg.Bad "Got too many inputs"))
      usage_msg;
    match !res with
    | None ->
        Arg.usage arg_spec_list usage_msg;
        exit 0
    | Some s -> s
end

let str = Command_line.parse ()

module Language_calc_Sign :
  Cairn.Parsing.parser_decorated with type value_parsed = Language.Ast.t =
struct
  type value_parsed = Language.Ast.t

  let error_strategy = Cairn.Parsing.Stop

  module Lexer = Language.Lexer
  module Parser = Language.Parser_calc
end

module Language_calc_Grammar = MenhirSdk.Cmly_read.Lift (struct
  let file_content = Option.get (Language.Cmly.read "Parser_calc.cmly")
  let prefix = "CMLY" ^ MenhirSdk.Version.version
  let grammar = Marshal.from_string file_content (String.length prefix)
end)

module ProgParser_calc =
  Cairn.Parsing.MakeWithDefaultMessage
    (Language_calc_Sign)
    (Language_calc_Grammar)

module Language_Sign :
  Cairn.Parsing.parser_decorated with type value_parsed = Language.Ast.t =
struct
  type value_parsed = Language.Ast.t

  let error_strategy = Cairn.Parsing.Stop

  module Lexer = Language.Lexer
  module Parser = Language.Parser
end

module Language_Grammar = MenhirSdk.Cmly_read.Lift (struct
  let file_content = Option.get (Language.Cmly.read "Parser.cmly")
  let prefix = "CMLY" ^ MenhirSdk.Version.version
  let grammar = Marshal.from_string file_content (String.length prefix)
end)

module ProgParser =
  Cairn.Parsing.MakeWithDefaultMessage (Language_Sign) (Language_Grammar)

module Language_simple_Sign :
  Cairn.Parsing.parser_decorated with type value_parsed = Language.Ast.t =
struct
  type value_parsed = Language.Ast.t

  let error_strategy = Cairn.Parsing.Stop

  module Lexer = Language.Lexer
  module Parser = Language.Parser_simple
end

module Language_simple_Grammar = MenhirSdk.Cmly_read.Lift (struct
  let file_content = Option.get (Language.Cmly.read "Parser_simple.cmly")
  let prefix = "CMLY" ^ MenhirSdk.Version.version
  let grammar = Marshal.from_string file_content (String.length prefix)
end)

module ProgParser_simple =
  Cairn.Parsing.MakeWithDefaultMessage
    (Language_simple_Sign)
    (Language_simple_Grammar)

(* si vous rajoutez un autre parseur, copiez les trois modules suivants et adaptez-les.*)
module Language_sandbox_Sign :
  Cairn.Parsing.parser_decorated with type value_parsed = Language.Ast.t =
struct
  type value_parsed = Language.Ast.t

  let error_strategy = Cairn.Parsing.Stop

  module Lexer = Language.Lexer
  module Parser = Language.Parser_sandbox
end

module Language_sandbox_Grammar = MenhirSdk.Cmly_read.Lift (struct
  let file_content = Option.get (Language.Cmly.read "Parser_sandbox.cmly")
  let prefix = "CMLY" ^ MenhirSdk.Version.version
  let grammar = Marshal.from_string file_content (String.length prefix)
end)

module ProgParser_sandbox =
  Cairn.Parsing.MakeWithDefaultMessage
    (Language_sandbox_Sign)
    (Language_sandbox_Grammar)

let text, lexbuf =
  if !Command_line.parse_from_file then MenhirLib.LexerUtil.read str
  else (str, Lexing.from_string str)

let main =
  (match !Command_line.parser with
  | Extended -> ProgParser.parse_interactive
  | Simple -> ProgParser_simple.parse_interactive
  | Calc -> ProgParser_calc.parse_interactive
  | Sandbox ->
      ProgParser_sandbox.parse_interactive
      (* si vous rajoutez un parseur, appelez le bon module ici*))
    text lexbuf

let () =
  match main with
  | Some prg ->
      Format.printf "@[<v 0>Program parsed:@,%a@,@]"
        Language.Ast.(pp_program true false)
        prg
  | None -> Format.printf "No program obtained\n"



{
    open Tokens
    exception Error of string
}

let digit = ['0'-'9']
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '\'' '_']

rule token = parse
    | "(*"              {commentary lexbuf}
    | [' ' '\t' '\r']   {token lexbuf}
    | '\n'              { Lexing.new_line lexbuf ; token lexbuf }
    | "if"              { IF }
    | "then"            { THEN }
    | "else"            { ELSE }
    | "+"               { ADD }
    | "-"               { SUB }
    | "*"               { MUL }
    | "/"               { DIV }
    | "%" | "mod"       { MOD }
    | "&&"              { AND }
    | "||"              { OR }
    | "!"               { NOT }
    | "="               { EQ }
    | "<>"              { NEQ }
    | "<="              { LEQ }
    | ">="              { GEQ }
    | "<"               { LT }
    | ">"               { GT }
    | "("               { L_PAR }
    | ")"               { R_PAR }
    | ";"               { SEMICOLON }
    | "true"            { BOOL(true) }
    | "false"           { BOOL(false) }
    | "print"           { PRINT }
    | "let"             { LET }
    | "rec"             { REC }
    | "fun"             { FUN }
    | "->"              { ARROW }
    | "^"               { CONCAT }
    | "::"              { CAT }
    | "hd"              { HEAD }
    | "tl"              { TAIL }
    | "in"              { IN }
    | "neg"             { NEG }
    | "["               { L_SQ }
    | "]"               { R_SQ }
    | "@"               { APPEND }

    | "\"" (([^ '\"'] | ('\\' '\"'))* as s) "\""  { STRING(s) }
    | (digit)+ as s     { INT(try int_of_string s with Failure _ ->(let pos = Lexing.lexeme_start_p lexbuf in raise (Error(Format.sprintf "Line %d, char %d ,Read: '%s'. It is not a valid integer" pos.pos_lnum (pos.pos_cnum - pos.pos_bol +1) s)) ))}
    | eof               { EOF }
    | ['a'-'z'] (alphanum)* as s  { ID(s) }
    | _ as s            { let pos = Lexing.lexeme_start_p lexbuf in raise (Error(Format.sprintf "Line %d, char %d ,Read: '%c'. It is not an acceptable character" pos.pos_lnum (pos.pos_cnum - pos.pos_bol +1) s)) }

and commentary = parse
    | '\n'      {Lexing.new_line lexbuf; commentary lexbuf}
    | "*)"      { token lexbuf }
    | _ { commentary lexbuf }
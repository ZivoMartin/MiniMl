%{
    open Ast
%}

%nonassoc IN ELSE ARROW
%left SEMICOLON

%start <Ast.t> main

%%

main:
| l = req_list EOF { l }

req_list:
| r = req l = req_list { r::l }
| r = req { [r] }

req:
| LET name = ID EQ e = expr { (false,name,e) }
| LET REC name = ID EQ e = expr { (true,name,e) }

expr:
| e = simple_expr { e }
| IF test = expr THEN th = expr ELSE el = expr { IfThenElse(test,th,el,Annotation.create $loc) }
| LET x = ID EQ e1 = expr IN e2 = expr { Let(false,x, e1 ,e2,Annotation.create $loc) }
| LET REC x = ID EQ e1 = expr IN e2 = expr { Let(true,x, e1 ,e2,Annotation.create $loc) }
| FUN x = ID ARROW e = expr { Fun(x,e,Annotation.create $loc) }
| e1 = expr SEMICOLON e2 = expr { Ignore(e1,e2,Annotation.create $loc) }
| e1 = app_expr e2 = simple_expr { App(e1,e2,Annotation.create $loc) } 

simple_expr:
| i = INT { Cst_i(i,Annotation.create $loc) }
| b = BOOL { Cst_b(b,Annotation.create $loc) }
| s = STRING { Cst_str(s,Annotation.create $loc) }
| f = built_in { Cst_func(f,Annotation.create $loc) }
| L_PAR R_PAR { Unit(Annotation.create $loc)}
| L_SQ R_SQ { Nil(Annotation.create $loc) }
| x = ID { Var(x,Annotation.create $loc) }
| L_PAR e = expr R_PAR { e }

app_expr:
| f = simple_expr { f }
| f = app_expr e = simple_expr { App(f,e,Annotation.create $loc)} 

%inline binop:
| ADD   { Add }
| SUB   { Sub }
| MUL   { Mul }
| DIV   { Div }
| MOD   { Mod }
| AND   { And }
| OR    { Or }
| EQ    { Eq }
| NEQ   { Neq }
| LT    { Lt }
| GT    { Gt }
| LEQ   { Leq }
| GEQ   { Geq }
| CONCAT { Concat }
| CAT   { Cat }
| APPEND { Append }

%inline built_in:
| L_PAR b = binop R_PAR { b }
| NEG   { UMin }
| NOT   { Not }
| HEAD  { Head }
| TAIL  { Tail }
| PRINT { Print }

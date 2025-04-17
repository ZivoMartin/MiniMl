%{
    open Ast
%}

%nonassoc IN ELSE ARROW
%left SEMICOLON

%start <Ast.t> main

%left OR
%left AND
%nonassoc EQ NEQ LT GT LEQ GEQ
%left ADD SUB
%left MUL DIV MOD
%right CONCAT CAT APPEND
%right UMINUS


%%

main:
| l = req_list EOF { l }

req_list:
| r = req l = req_list { r::l }
| r = req { [r] }

req:
| LET name = ID EQ e = expr { (false,name,e) }
| LET REC name = ID EQ e = expr { (true,name,e) }
| LET name = ID e = sugar_func_decl { (false,name,e) }
| LET REC name = ID e = sugar_func_decl { (true,name,e) }

sugar_func_decl:
| arg = ID EQ e = expr { Fun(arg, e, Annotation.create $loc) }
| arg = ID e = sugar_func_decl { Fun(arg, e, Annotation.create $loc) }


expr:
| IF test = expr THEN th = expr ELSE el = expr
    { IfThenElse(test, th, el, Annotation.create $loc) }

| LET x = ID EQ e1 = expr IN e2 = expr
    { Let(false, x, e1, e2, Annotation.create $loc) }
| LET REC x = ID EQ e1 = expr IN e2 = expr
    { Let(true, x, e1, e2, Annotation.create $loc) }
| LET x = ID e1 = sugar_func_decl IN e2 = expr
    { Let(false, x, e1, e2, Annotation.create $loc) }
| LET REC x = ID e1 = sugar_func_decl IN e2 = expr
    { Let(true, x, e1, e2, Annotation.create $loc) }

| FUN x = ID ARROW e = expr
    { Fun(x, e, Annotation.create $loc) }
| e1 = expr SEMICOLON e2 = expr
    { Ignore(e1, e2, Annotation.create $loc) }
| e1 = expr op = binop e2 = expr
    { App(App(Cst_func(op, Annotation.create $loc), e1, Annotation.create $loc), e2, Annotation.create $loc) }

| SUB e = expr %prec UMINUS
    { App(Cst_func(UMin, Annotation.create $loc), e, Annotation.create $loc) }
| e1 = expr e2 = simple_expr
    { App(e1, e2, Annotation.create $loc) }
| e = simple_expr
    { e }


simple_expr:
| i = INT { Cst_i(i,Annotation.create $loc) }
| b = BOOL { Cst_b(b,Annotation.create $loc) }
| s = STRING { Cst_str(s,Annotation.create $loc) }
| f = built_in { Cst_func(f,Annotation.create $loc) }
| L_PAR R_PAR { Unit(Annotation.create $loc)}
| L_SQ R_SQ { Nil(Annotation.create $loc) }
| x = ID { Var(x,Annotation.create $loc) }
| L_PAR e = expr R_PAR { e }
| L_SQ e = list_builder { e }

list_builder:
| e = simple_expr R_SQ { App(App(Cst_func(Cat, Annotation.create $loc), e, Annotation.create $loc), Nil(Annotation.create $loc), Annotation.create $loc) }
| e = simple_expr SEMICOLON  rest = list_builder { App(App(Cst_func(Cat, Annotation.create $loc), e, Annotation.create $loc), rest, Annotation.create $loc) }

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

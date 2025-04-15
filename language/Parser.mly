%{
    open Ast
%}

%nonassoc IN ELSE ARROW
%left SEMICOLON

%left ADD SUB
%left MUL DIV MOD
%nonassoc UMIN

%start <Ast.t> main

%%

main:
| l = req_list EOF { l }

req_list:
| r = req l = req_list { r :: l }
| r = req { [r] }

req:
| LET name = ID EQ e = expr { (false, name, e) }
| LET REC name = ID EQ e = expr { (true, name, e) }
| LET name = ID e = sugar_func_decl { (false, name, e) }
| LET REC name = ID e = sugar_func_decl { (true, name, e) }

sugar_func_decl:
| arg = ID EQ e = expr { Fun(arg, e, Annotation.create $loc) }
| arg = ID e = sugar_func_decl { Fun(arg, e, Annotation.create $loc) }

expr:
| e1 = expr SEMICOLON e2 = expr { Ignore(e1, e2, Annotation.create $loc) }
| IF test = expr THEN th = expr ELSE el = expr { IfThenElse(test, th, el, Annotation.create $loc) }
| LET x = ID EQ e1 = expr IN e2 = expr { Let(false, x, e1, e2, Annotation.create $loc) }
| LET REC x = ID EQ e1 = expr IN e2 = expr { Let(true, x, e1, e2, Annotation.create $loc) }
| LET x = ID e1 = sugar_func_decl IN e2 = expr { Let(false, x, e1, e2, Annotation.create $loc) }
| LET REC x = ID e1 = sugar_func_decl IN e2 = expr { Let(true, x, e1, e2, Annotation.create $loc) }
| FUN x = ID ARROW e = expr { Fun(x, e, Annotation.create $loc) }
| e = or_expr { e }

or_expr:
| lhs = or_expr OR rhs = and_expr { App(App(Cst_func(Or, Annotation.create $loc), lhs, Annotation.create $loc), rhs, Annotation.create $loc) }
| e = and_expr { e }

and_expr:
| lhs = and_expr AND rhs = eq_expr { App(App(Cst_func(And, Annotation.create $loc), lhs, Annotation.create $loc), rhs, Annotation.create $loc) }
| e = eq_expr { e }

eq_expr:
| lhs = eq_expr EQ rhs = rel_expr { App(App(Cst_func(Eq, Annotation.create $loc), lhs, Annotation.create $loc), rhs, Annotation.create $loc) }
| lhs = eq_expr NEQ rhs = rel_expr { App(App(Cst_func(Neq, Annotation.create $loc), lhs, Annotation.create $loc), rhs, Annotation.create $loc) }
| e = rel_expr { e }

rel_expr:
| lhs = rel_expr LT rhs = add_expr { App(App(Cst_func(Lt, Annotation.create $loc), lhs, Annotation.create $loc), rhs, Annotation.create $loc) }
| lhs = rel_expr LEQ rhs = add_expr { App(App(Cst_func(Leq, Annotation.create $loc), lhs, Annotation.create $loc), rhs, Annotation.create $loc) }
| lhs = rel_expr GT rhs = add_expr { App(App(Cst_func(Gt, Annotation.create $loc), lhs, Annotation.create $loc), rhs, Annotation.create $loc) }
| lhs = rel_expr GEQ rhs = add_expr { App(App(Cst_func(Geq, Annotation.create $loc), lhs, Annotation.create $loc), rhs, Annotation.create $loc) }
| e = add_expr { e }

add_expr:
| lhs = add_expr ADD rhs = mul_expr { App(App(Cst_func(Add, Annotation.create $loc), lhs, Annotation.create $loc), rhs, Annotation.create $loc) }
| lhs = add_expr SUB rhs = mul_expr { App(App(Cst_func(Sub, Annotation.create $loc), lhs, Annotation.create $loc), rhs, Annotation.create $loc) }
| e = mul_expr { e }

mul_expr:
| lhs = mul_expr MUL rhs = app_expr { App(App(Cst_func(Mul, Annotation.create $loc), lhs, Annotation.create $loc), rhs, Annotation.create $loc) }
| lhs = mul_expr DIV rhs = app_expr { App(App(Cst_func(Div, Annotation.create $loc), lhs, Annotation.create $loc), rhs, Annotation.create $loc) }
| lhs = mul_expr MOD rhs = app_expr { App(App(Cst_func(Mod, Annotation.create $loc), lhs, Annotation.create $loc), rhs, Annotation.create $loc) }
| e = app_expr { e }

app_expr:
| f = app_expr a = simple_expr { App(f, a, Annotation.create $loc) }
| f = simple_expr { f }

simple_expr:
| SUB e = simple_expr { App(Cst_func(UMin, Annotation.create $loc), e, Annotation.create $loc) }
| i = INT { Cst_i(i, Annotation.create $loc) }
| b = BOOL { Cst_b(b, Annotation.create $loc) }
| s = STRING { Cst_str(s, Annotation.create $loc) }
| f = built_in { Cst_func(f, Annotation.create $loc) }
| x = ID { Var(x, Annotation.create $loc) }
| L_PAR R_PAR { Unit(Annotation.create $loc) }
| L_SQ R_SQ { Nil(Annotation.create $loc) }
| L_PAR e = expr R_PAR { e }
| L_SQ e = list_builder { e }

list_builder:
| e = expr R_SQ { App(App(Cst_func(Cat, Annotation.create $loc), e, Annotation.create $loc), Nil(Annotation.create $loc), Annotation.create $loc) }
| e = expr SEMICOLON rest = list_builder { App(App(Cst_func(Cat, Annotation.create $loc), e, Annotation.create $loc), rest, Annotation.create $loc) }

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

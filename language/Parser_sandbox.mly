%{
    open Ast
%}

%start <Ast.t> main

%%

main:
e = expr EOF { [(false,"result",e)] }

expr:
| e = simple_expr { e }
| FUN x = ID ARROW e = expr { Fun(x,e,Annotation.create $loc) }
| e1 = app_expr e2 = simple_expr { App(e1,e2,Annotation.create $loc) } 

simple_expr:
| i = INT { Cst_i(i,Annotation.create $loc) }
| f = built_in { Cst_func(f,Annotation.create $loc) }
| x = ID { Var(x,Annotation.create $loc) }
| L_PAR e = expr R_PAR { e }

app_expr:
| f = simple_expr { f }
| f = app_expr e = simple_expr { App(f,e,Annotation.create $loc)} 

%inline binop:
| ADD   { Add }

%inline built_in:
| L_PAR b = binop R_PAR { b }
| NEG   { UMin }
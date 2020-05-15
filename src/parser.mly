%token <int> INT
%token <string> VAR
%token LPAREN
%token RPAREN
%token IF
%token EOF

%start <Exp.t> f

%%

f : e = expr; EOF { e }

expr :
| n = INT; { Exp.Int n }
| s = VAR; { Exp.Var s }
| LPAREN; e = expr; es = list (expr); RPAREN { Exp.Call (e, es) }
| LPAREN; IF; e1 = expr; e2 = expr; e3 = expr; RPAREN { Exp.If (e1, e2, e3) }
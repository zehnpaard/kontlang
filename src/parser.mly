%token <int> INT
%token <string> VAR
%token LPAREN
%token RPAREN
%token EOF

%start <Exp.t> f

%%

f : e = expr; EOF { e }

expr :
| n = INT; { Exp.Int n }
| s = VAR; { Exp.Var s }
| LPAREN; e = expr; es = list (expr); RPAREN { Exp.Call (e, es) }
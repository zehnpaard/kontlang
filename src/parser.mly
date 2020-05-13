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
| LPAREN; es = nonempty_list (expr); RPAREN { Exp.Call es }
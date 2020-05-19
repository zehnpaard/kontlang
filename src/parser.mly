%token <int> INT
%token <string> VAR
%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK
%token IF
%token LET
%token LETS
%token EOF

%start <Exp.t> f

%%

f : e = expr; EOF { e }

expr :
| n = INT; { Exp.Int n }
| s = VAR; { Exp.Var s }
| LPAREN; e = expr; es = list (expr); RPAREN { Exp.Call (e, es) }
| LPAREN; IF; e1 = expr; e2 = expr; e3 = expr; RPAREN { Exp.If (e1, e2, e3) }
| LPAREN; LET; LBRACK; v1 = VAR; e1 = expr; RBRACK; e2 = expr; RPAREN { Exp.Let ([(v1, e1)], e2) }
| LPAREN; LET; LBRACK; ves = nonempty_list(var_exp); RBRACK; e2 = expr; RPAREN { Exp.Let (ves, e2) }
| LPAREN; LETS; LBRACK; ves = nonempty_list(var_exp); RBRACK; e2 = expr; RPAREN { Exp.Lets (ves, e2) }

var_exp :
| LPAREN; v = VAR; e = expr; RPAREN { (v, e) }
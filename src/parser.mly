%token <int> INT
%token <string> VAR
%token <string> STR
%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK
%token IF
%token COND
%token LET
%token LETS
%token FN
%token LETFN
%token LETREC
%token MACRO
%token DO
%token RESET
%token SHIFT
%token DEFINE
%token MODULE
%token EOF

%start <Exp.t> f

%%

f : e = expr; EOF { e }

expr :
| n = INT; { Exp.Int n }
| s = VAR; { Exp.Var s }
| s = STR; { Exp.Str s }
| LPAREN; e = expr; es = list (expr); RPAREN { Exp.Call(e, es) }
| LPAREN; IF; e1 = expr; e2 = expr; e3 = expr; RPAREN { Exp.If(e1, e2, e3) }
| LPAREN; COND; ees = nonempty_list(cond_pair); RPAREN { Exp.Cond ees }
| LPAREN; LET; LBRACK; v1 = VAR; e1 = expr; RBRACK; e2 = expr; RPAREN { Exp.Let([(v1, e1)], e2) }
| LPAREN; LET; LBRACK; ves = nonempty_list(var_exp); RBRACK; e2 = expr; RPAREN { Exp.Let(ves, e2) }
| LPAREN; LETS; LBRACK; ves = nonempty_list(var_exp); RBRACK; e2 = expr; RPAREN { Exp.Lets(ves, e2) }
| LPAREN; FN; LBRACK; ss = list(VAR); RBRACK; e = expr; RPAREN { Exp.Fn(ss, e) }
| LPAREN; LETFN; LBRACK; fn = func; RBRACK; e = expr; RPAREN
    { Exp.LetFn([fn], e) }
| LPAREN; LETFN; LBRACK; fns = list(funcp); RBRACK; e = expr; RPAREN
    { Exp.LetFn(fns, e) }
| LPAREN; LETREC; LBRACK; fn = func; RBRACK; e = expr; RPAREN
    { Exp.LetRec([fn], e) }
| LPAREN; LETREC; LBRACK; fns = list(funcp); RBRACK; e = expr; RPAREN
    { Exp.LetRec(fns, e) }
| LPAREN; MACRO; LBRACK; ss = list(VAR); RBRACK; e = expr; RPAREN { Exp.Macro(ss, e) }
| LPAREN; DO; LBRACK; es = nonempty_list(expr); RBRACK; RPAREN; { Exp.Do es }
| LPAREN; RESET; e = expr; RPAREN { Exp.Reset e }
| LPAREN; SHIFT; LBRACK; s = VAR; RBRACK; e = expr; RPAREN { Exp.Shift(s, e) }
| LPAREN; MODULE; LBRACK; es = nonempty_list(module_expr); RBRACK; RPAREN { Exp.Module es }

module_expr :
| expr
| LPAREN; DEFINE; s = VAR; e = expr; RPAREN { Exp.Define(s, e) }

var_exp :
| LPAREN; v = VAR; e = expr; RPAREN { (v, e) }

func :
| fname = VAR; LBRACK; ss = list(VAR); RBRACK; body = expr; { (fname, ss, body) }

funcp :
| LPAREN; fn = func; RPAREN { fn }

cond_pair :
| LBRACK; e1 = expr; e2 = expr; RBRACK { (e1, e2) }
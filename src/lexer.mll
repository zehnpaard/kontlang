let whitespace = [' ' '\t' '\n']
let digit = ['0'-'9']
let char = ['a'-'z' 'A'-'Z' '+' '-' '*' '/' '<' '>' '=' '!' '?' '_']
let number = ('0'|['1'-'9'] digit*)
let variable = char (char|digit)*
let string = '"' + (' '|char|number)* '"'

rule f = parse
  | whitespace* { f lexbuf }
  | number as n { Parser.INT (int_of_string n) }
  | string as s { Parser.STR (String.sub s 1 @@ String.length s - 2) }
  | "(" { Parser.LPAREN }
  | ")" { Parser.RPAREN }
  | "[" { Parser.LBRACK }
  | "]" { Parser.RBRACK }
  | "if" { Parser.IF }
  | "cond" { Parser.COND }
  | "let" { Parser.LET }
  | "let*" { Parser.LETS }
  | "fn" { Parser.FN }
  | "letfn" { Parser.LETFN }
  | "letrec" { Parser.LETREC }
  | variable as s { Parser.VAR s }
  | eof { EOF }
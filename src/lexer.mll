let whitespace = [' ' '\t' '\n']
let digit = ['0'-'9']
let char = ['a'-'z' 'A'-'Z' '+' '-' '*' '/' '<' '>' '=' '!' '?' '_']
let number = ('0'|['1'-'9'] digit*)
let variable = char (char|digit)*
let mvariable = variable '.' variable ('.' variable)*
let string = '"' + (' '|char|number|'.'|'\\')* '"'

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
  | "macro" { Parser.MACRO }
  | "do" { Parser.DO }
  | "reset" { Parser.RESET }
  | "shift" { Parser.SHIFT }
  | "define" { Parser.DEFINE }
  | "module" { Parser.MODULE }
  | "import" { Parser.IMPORT }
  | mvariable as s { Parser.MVAR (String.split_on_char '.' s) }
  | variable as s { Parser.VAR s }
  | eof { EOF }
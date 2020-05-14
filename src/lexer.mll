let whitespace = [' ' '\t' '\n']
let digit = ['0'-'9']
let char = ['a'-'z' 'A'-'Z' '+' '-' '*' '/' '<' '>' '=' '!' '?' '_']
let number = ('0'|['1'-'9'] digit*)
let variable = char (char|digit)*

rule f = parse
  | whitespace* { f lexbuf }
  | number as n { Parser.INT (int_of_string n) }
  | "(" { Parser.LPAREN }
  | ")" { Parser.RPAREN }
  | variable as s { Parser.VAR s }
  | eof { EOF }
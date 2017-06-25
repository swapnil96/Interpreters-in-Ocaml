{
  open Parser
}
rule token = parse
  | eof                     { EOF }
  | [' ' '\t' '\n']         { token lexbuf }
  | '('                     { LPAREN }
  | ')'                     { RPAREN }
  | "+"                     { ADD }
  | "-"                     { SUB }
  | "*"                     { MUL }
  | "fun"                   { FUNC }
  | "->"                    { ARROW }
  | "="                     { EQUAL }
  | "let"                   { LET }
  | "let rec"               { LETREC }
  | "in"                    { IN }
  | ['0'-'9']+  as p        { INT (p) }
  | ['a'-'z']+  as p        { IDENTITY (p) }

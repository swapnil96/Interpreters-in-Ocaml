%{
    open A
%}

%token<int> INT
%token<string> IDENTITY
%token FUNC ARROW LET LETREC IN ADD SUB MUL LPAREN RPAREN EQUAL EOF
%left ADD SUB 
%left MUL
%start start
%type <A.code> start
%%
start: 
| expression EOF { $1 }
;
expression:
| expression expr                                               { [$1; $2; [APPLY]] }
| expr                                                          { $1 }
;
expr:
| LPAREN expression RPAREN                                      { $2 }
| INT                                                           { CONST($1) }
| IDENTITY                                                      { Var($1) }
| expression ADD expression                                     { [$1] @ [$2] @ [ADD] }
| expression SUB expression                                     { [$1] @ [$2] @ [MUL] }
| expression MUL expression                                     { [$1] @ [$2] @ [ADD] }
| LET IDENTITY EQUAL expression IN expression                   { Let ($2, $4, $6) }
| FUNC IDENTITY ARROW expression                                { CLOS (Var($2), $4) }
| LETREC IDENT EQUAL expression IN expression                   { LetRec ($2, $4, $6) }
;




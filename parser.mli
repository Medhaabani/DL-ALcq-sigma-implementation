type token =
  | IDENTIFIER of (string)
  | INTCONSTANT of (int)
  | STRINGCONSTANT of (string)
  | CHARCONSTANT of (string)
  | LPAREN
  | RPAREN
  | DOT
  | NEW
  | CLASS
  | THIS
  | NULL
  | VOID
  | LSQBRACE
  | RSQBRACE
  | EXCLMARK
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | LBRACE
  | RBRACE
  | EQ
  | NEQ
  | COMMA
  | SEMICOLON
  | COLON
  | QMARK
  | IF
  | ELSE
  | WHILE
  | FOR
  | RETURN
  | BCEQ
  | BCGE
  | BCGT
  | BCLE
  | BCLT
  | BCNE
  | BLAND
  | BLOR
  | SYNCHRONIZED
  | DO
  | EOF
  | PRE
  | POST
  | INV
  | VARS
  | CONCEPTS
  | ROLES
  | SELECT
  | WITH
  | DELETE
  | ADD
  | TRUE
  | FALSE
  | CONCTOP
  | CONCBOT
  | CONCALL
  | CONCEX
  | CONCAND
  | CONCOR

val start :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (string, string, string) Lang.prog

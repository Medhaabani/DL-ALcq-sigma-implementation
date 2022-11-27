{

  open Lexing
  open Parser
  open Lang
  exception Lexerror


  let pos lexbuf = (lexeme_start lexbuf, lexeme_end lexbuf)

  let advance_line_pos pos =
    { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum; }

  let advance_line lexbuf =
    lexbuf.lex_curr_p <- advance_line_pos lexbuf.lex_curr_p

}

let alph =           ['_''a'-'z''A'-'Z']
let num  =           ['0'-'9'] 
let decimal	=	'0'|(['1'-'9']['0'-'9']*)
let ml_comment = '/' '*' [^'*']* ('*' (('*'*)|([^'*''/'][^'*']*)))* '*' '/'
let sl_comment = '/' '/' ([^'@''\n'] [^'\n']*)? '\n' 
let importline = "import" [^';']*';' 
let packageDecl = "package" [^';']*';'
let stringLiteral='\"' [^'\"']* ('\\' '\"' [^'\"']*)* '\"'
let charLiteral='\'' [^'\''] '\''

rule token = parse
 [' ' '\t']
    { token lexbuf }    (* white space: recursive call of lexer *)
|'\n'
    {advance_line lexbuf; token lexbuf }    (* white space: recursive call of lexer *)
|importline
    { token lexbuf } (*import directives ignored*)
|packageDecl
    { token lexbuf } (*package declaration ignored*)
| ml_comment
    { token lexbuf }    (* comment --> ignore *)
| sl_comment
    { token lexbuf }    (* comment --> ignore *)
| decimal  as i	  { INTCONSTANT (int_of_string i)}
| stringLiteral as i {STRINGCONSTANT (String.sub i 1 (String.length i -2))}
(*delete escaped quotes slash-quote around string literal*)
| charLiteral as i {CHARCONSTANT (i)}
| '+'  { PLUS }
| '-'  { MINUS }
| '*'  { TIMES }
| '/'  { DIV }
| '%'  { MOD }
| '('  { LPAREN }
| ')'  { RPAREN }
| '{'  { LBRACE }
| '}'  { RBRACE }
| '['  { LSQBRACE }
| ']'  { RSQBRACE }
| '='  { EQ }
| "!="  { NEQ }
| ','  { COMMA }
| ';'  { SEMICOLON }
| ':'  { COLON }
| '?'  { QMARK }
| '.'  { DOT }
| '!'  { EXCLMARK }

| "[1]"  { CONCTOP}
| "[0]"  { CONCBOT}
| "[!]"  { CONCALL}
| "[?]"  { CONCEX}
| "[&&]" { CONCAND}
| "[||]" { CONCOR}

| "true"       {TRUE}
| "false"      {FALSE}
| "if"         {IF}
| "else"       {ELSE}
| "while"      {WHILE}
| "for"        {FOR}

(* New *)

| "vars"       {VARS}
| "concepts"   {CONCEPTS}
| "roles"      {ROLES}

| "pre"        {PRE}
| "post"       {POST}
| "inv"        {INV}

| "select"     {SELECT}
| "with"       {WITH}
| "delete"     {DELETE}
| "add"        {ADD}

(* End New *)

| "=="         {BCEQ}
| ">="         {BCGE}
| '>'          {BCGT}
| "<="         {BCLE}
| '<'          {BCLT}
| "!="         {BCNE}

| "&&"         {BLAND}
| "||"         {BLOR}

| eof          {EOF}

| alph(alph|num)* as i  {IDENTIFIER i}

| _  {Printf.printf "ERROR: unrecogized symbol '%s'\n" (Lexing.lexeme lexbuf);
      raise Lexerror }

and
    ruleTail acc = parse
| eof { acc }
| _* as str { ruleTail (acc ^ str) lexbuf }

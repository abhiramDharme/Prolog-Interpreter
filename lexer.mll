{
  open Parser;;
  exception InvalidToken of char;;
}

rule token = parse
    eof                   {EOF}
  | [' ' '\t' '\n']+          {token lexbuf}
  | ['A'-'Z']['A'-'Z' 'a'-'z' '0'-'9' '_']* as v         {VAR(v)}
  | ['a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']* | ("\"" [^ '\"']+ "\"") as c         {CONSTANT(c)}
  | ['-']? '0' | ['-']? ['1'-'9']['0'-'9']* | ['-']? ['1'-'9']['0'-'9']* '.' ['0'-'9']+ | ['-']? '0' '.' ['0'-'9']+ | ['+']? '0' | ['+']? ['1'-'9']['0'-'9']* | ['+']? ['1'-'9']['0'-'9']* '.' ['0'-'9']+ | ['+']? '0' '.' ['0'-'9']+ as n          {NUMERAL(float_of_string n)}
  | '('                   {LPAREN}
  | ')'                   {RPAREN}
  | '['                   {LLIST}
  | ']'                   {RLIST}
  | ','                   {COMMA}
  | '='                   {EQUAL}
  | '+'                   {ADD}
  | '-'                   {SUB}
  | '*'                   {TIMES}
  | '/'                   {DIV}
  | '>'                   {GT}
  | ">="                  {GE}
  | '<'                   {LT}
  | "<="                  {LE}
  | "\\="                 {NOT_EQUAL}
  | '|'                   {BAR}
  | '!'                   {CUT}
  | '.'                   {END}
  | ":-"                  {IF}
  | '%' [^ '\n']* '\n' | '%' [^ '\n']* eof   {token lexbuf}
  | "/*" ( [^ '*']* '*' )* [^ '/']* "*/"    {token lexbuf}
  | _ as s                {raise (InvalidToken s)}
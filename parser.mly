%{
    open Interpreter;;
%}

%token <string> VAR CONSTANT
%token <float> NUMERAL
%token ADD SUB LLIST IF BAR RLIST COMMA EQUAL TIMES RPAREN  NOT_EQUAL END CUT DIV GT LT EOF LPAREN GE LE

%left PLUS MINUS TIMES DIV COMMA BAR LPAREN RPAREN LLIST RLIST
%nonassoc EQUAL NOT_EQUAL END CUT IF GT LT


%start program goal
%type <Ast.program> program
%type <Ast.goal> goal
%type <Ast.clause list> clause_list
%type <Ast.clause> clause
%type <Ast.atom list> atomic_list
%type <Ast.atom> atom
%type <Ast.term list> term_list
%type <Ast.term> term
%type <Ast.term> list
%type <Ast.term> list_body
%%

list_body:
    term                                {Node("list", [$1; Node("empty", [])])}
  | term COMMA list_body                {Node("list", [$1; $3])}
  | term BAR term                       {Node("list", [$1; $3])}
;

list:
    LLIST RLIST                         {Node("empty", [])}
  | LLIST list_body RLIST               {$2}
;

term:
  | LPAREN term RPAREN                  {$2}
  | VAR                                 {Var($1)}
  | CONSTANT                            {Node($1, [])}
  | NUMERAL                             {Num($1)}
  | CONSTANT LPAREN term_list RPAREN    {Node($1, $3)}
  | term ADD term                       {Node("+", [$1; $3])}
  | term SUB term                       {Node("-", [$1; $3])}
  | term TIMES term                     {Node("*", [$1; $3])}
  | term DIV term                       {Node("/", [$1; $3])}
  | list                                {$1}
;

term_list:
  | term                                {[$1]}
  | term COMMA term_list                {($1)::$3}
;

atom:
  | CONSTANT LPAREN term_list RPAREN    {Atom($1, $3)}
  | CONSTANT                            {Atom($1, [])}
  | term EQUAL term                     {Atom("equal", [$1; $3])}
  | term NOT_EQUAL term                 {Atom("not_equal", [$1; $3])}
  | term LT term                        {Atom("<", [$1; $3])}
  | term GT term                        {Atom(">", [$1; $3])}
  | term LE term                        {Atom("<=", [$1; $3])}
  | term GE term                        {Atom(">=", [$1; $3])}
  | CUT                                 {Atom("cut", [])}
;

atomic_list:
  | atom COMMA atomic_list              {($1)::$3}
  | atom                                {[$1]}
;

goal:
  | atomic_list END                     {Goal($1)}
;

clause:
  | atom IF atomic_list END             {Rule(Head($1), Body($3))}
  | atom END                            {Fact(Head($1))}
;

clause_list:
  | clause                              {[$1]}
  | clause clause_list                  {($1)::$2}
;

program:
  | clause_list EOF                     {$1}
  | EOF                                 {[]}
;
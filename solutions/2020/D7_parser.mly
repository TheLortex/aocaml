
%token <int> INT
%token DOT COMMA
%token <string> DESC
%token CONTAIN BAG NO_OTHER
%token EOF

%start <(string * ((string * int) list)) list> main

%%

main:
| stmt = statement EOF { [stmt] }
| stmt = statement m = main { stmt :: m}

statement:
| e = expr DOT { e }

expr:
| t1 = DESC t2 = DESC BAG CONTAIN d = desc
    { (t1 ^ "-" ^ t2, d) }

desc:
| NO_OTHER BAG { [] }
| d = desc_multi { d }

desc_multi:
| n = INT d1 = DESC d2 = DESC BAG { [(d1 ^ "-" ^ d2,n)]}
| n = INT d1 = DESC d2 = DESC BAG COMMA next = desc_multi { (d1 ^ "-" ^ d2,n)::next } 

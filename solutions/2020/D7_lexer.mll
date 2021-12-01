
{
  open D7_parser

  exception Error of string
}

rule token = parse
| [' ' '\t' '\n']
    { token lexbuf }
| '.'
    { DOT }
| ','
    { COMMA }
| ['0'-'9']+ as i
    { INT (int_of_string i) }
| "bags" | "bag"
    { BAG }
| "contain"
    { CONTAIN }
| "no other"
    { NO_OTHER }
| ['a'-'z']+ as i
    { DESC i}
| eof
    { EOF }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

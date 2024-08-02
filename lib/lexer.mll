{
open Parser

exception Lexing_error of string
}


let linecomment = "//"[^ '\n']+
let multilinecomment = "/*"_*?"*/"
let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let lower = ['a'-'z']
let upper = ['A'-'Z']
let letter = (lower | upper)
let var = (lower | '_') (letter | digit | '_' | ''')*
let xvar = upper (letter | digit | '_' | ''')*

rule comment = parse
    | "\n" {Lexing.new_line lexbuf; comment lexbuf}
    | "*/" {read lexbuf}
    | _ {comment lexbuf}

and read = parse
    | "\n" {Lexing.new_line lexbuf; read lexbuf}
    | "/*" {comment lexbuf}
    | linecomment
    | white {read lexbuf}
    | "()" {NULL}
    | "(" {LPAREN}
    | ")" {RPAREN}
    | "{" {LBRACE}
    | "}" {RBRACE}
    | "[" {LBRACKET}
    | "]" {RBRACKET}
    | "," {COMMA}
    | ";" {SEMICOLON}
    | "ctrl" {CTRL}
    | "match" {MATCH}
    | "pmatch" {PMATCH}
    | "try" {TRY}
    | "catch" {CATCH}
    | "lambda" {LAMBDA}
    | "let" {LET}
    | "in" {IN}
    | "|>" {PIPE}
    | "->" {ARROW}
    | "leq" {LEQ}
    | "geq" {GEQ}
    | "lt" {LT}
    | "gt" {GT}
    | "<" {LANGLE}
    | ">" {RANGLE}
    | "of" {OF}
    | "void" {VOID}
    | "qunit" {QUNIT}
    | "u3" {U3}
    | "left" {LEFT}
    | "right" {RIGHT}
    | "rphase" {RPHASE}
    | "gphase" {GPHASE}
    | int { CONST (int_of_string (Lexing.lexeme lexbuf)) }
    | "pi" {PI}
    | "euler" {EULER}
    | "+" {PLUS}
    | "-" {MINUS}
    | "*" {TIMES}
    | "/" {DIV}
    | "^" {POW}
    | "sin" {SIN}
    | "cos" {COS}
    | "tan" {TAN}
    | "arcsin" {ARCSIN}
    | "arccos" {ARCCOS}
    | "arctan" {ARCTAN}
    | "exp" {EXP}
    | "ln" {LN}
    | "log2" {LOG2}
    | "sqrt" {SQRT}
    | "ceil" {CEIL}
    | "floor" {FLOOR}
    | "%" {MOD}
    | "def" {DEF}
    | ":=" {DEFSTART}
    | "=" {EQUAL}
    | "end" {END}
    | "if" {IF}
    | "then" {THEN}
    | "else" {ELSE}
    | "endif" {ENDIF}
    | "fail" {FAIL}
    | var { VAR (Lexing.lexeme lexbuf) }
    | xvar { XVAR (Lexing.lexeme lexbuf) }
    | eof {EOF}
    | _ as c { raise (Lexing_error (Printf.sprintf "Unexpected character: %c" c)) }

{
open Parser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = '-'? digit+
let lower = ['a'-'z']
let upper = ['A'-'Z']
let letter = (lower | upper)
let var = lower (lower | digit | '_' | ''')*
let xvar = upper (letter | digit | '_' | ''')*

rule read =
    parse
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
    | "try" {TRY}
    | "catch" {CATCH}
    | "lambda" {LAMBDA}
    | "let" {LET}
    | "in" {IN}
    | "|>" {PIPE}
    | "->" {ARROW}
    | "<" {LANGLE}
    | ">" {RANGLE}
    | "of" {OF}
    | "void" {VOID}
    | "qunit" {QUNIT}
    | "u3" {U3}
    | "left" {LEFT}
    | "right" {RIGHT}
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
    | "sqrt" {SQRT}
    | "def" {DEF}
    | ":=" {DEFSTART}
    | "=" {EQUAL}
    | "end" {END}
    | "if" {IF}
    | "then" {THEN}
    | "else" {ELSE}
    | "endif" {ENDIF}
    | var { VAR (Lexing.lexeme lexbuf) }
    | xvar { XVAR (Lexing.lexeme lexbuf) }
    | eof {EOF}

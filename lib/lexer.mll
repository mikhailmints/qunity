{
open Parser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z' '_']
let var = letter (letter | digit)*

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
    | "ctrl" {CTRL}
    | "try" {TRY}
    | "catch" {CATCH}
    | "lambda" {LAMBDA}
    | "let" {LET}
    | "in" {IN}
    | "=" {EQUAL}
    | "|>" {PIPE}
    | "->" {ARROW}
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
    | "sin" {SIN}
    | "cos" {COS}
    | "tan" {TAN}
    | "arcsin" {ARCSIN}
    | "arccos" {ARCCOS}
    | "arctan" {ARCTAN}
    | "exp" {EXP}
    | "ln" {LN}
    | "sqrt" {SQRT}
    | var { VAR (Lexing.lexeme lexbuf) }
    | eof {EOF}

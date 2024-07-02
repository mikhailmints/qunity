{
open Parser
}


let linecomment = "//"[^ '\n']+
let multilinecomment = "/*"_*?"*/"
let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = '-'? digit+
let lower = ['a'-'z']
let upper = ['A'-'Z']
let letter = (lower | upper)
let var = (lower | '_') (lower | digit | '_' | ''')*
let xvar = upper (letter | digit | '_' | ''')*

rule comment = parse
    | "*/" {read lexbuf}
    | _ {comment lexbuf}

and read = parse
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
    | "sqrt" {SQRT}
    | "round" {ROUND}
    | "%" {MOD}
    | "def" {DEF}
    | ":=" {DEFSTART}
    | "=" {EQUAL}
    | "end" {END}
    | "if" {IF}
    | "then" {THEN}
    | "else" {ELSE}
    | "endif" {ENDIF}
    (* | string {
        let s = Lexing.lexeme lexbuf in
        STRING (String.sub s 1 (String.length s))
        } *)
    | var { VAR (Lexing.lexeme lexbuf) }
    | xvar { XVAR (Lexing.lexeme lexbuf) }
    | eof {EOF}

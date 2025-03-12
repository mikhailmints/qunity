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
let alpha = (letter | digit | '_' | ''')
let qvar = (lower | '_') alpha*
let typename = upper alpha*
let typevar = ''' alpha*
let realvar = '#' alpha+
let progvar = '@' alpha+
let exprvar = '$' alpha+

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
    | "|" {VBAR}
    | ":" {COLON}
    | "of" {OF}
    | "Void" {VOID}
    | "Unit" {UNIT}
    | "u3" {U3}
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
    | "end" {END}
    | "=" {EQUAL}
    | "!=" {NEQ}
    | "<" {LT}
    | "<=" {LEQ}
    | ">" {GT}
    | ">=" {GEQ}
    | "&&" {AND}
    | "||" {OR}
    | "!" {NOT}
    | "type" {TYPE}
    | "if" {IF}
    | "then" {THEN}
    | "else" {ELSE}
    | "endif" {ENDIF}
    | qvar { QVAR (Lexing.lexeme lexbuf) }
    | typename { TYPENAME (Lexing.lexeme lexbuf) }
    | typevar { TYPEVAR (Lexing.lexeme lexbuf) }
    | exprvar { EXPRVAR (Lexing.lexeme lexbuf) }
    | progvar { PROGVAR (Lexing.lexeme lexbuf) }
    | realvar { REALVAR (Lexing.lexeme lexbuf) }
    | eof {EOF}
    | _ as c { raise (Lexing_error (Printf.sprintf "Unexpected character: %c" c)) }

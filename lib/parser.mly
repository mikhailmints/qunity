%{
open Reals
open Syntax
%}

%token NULL
%token <string> VAR
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token COMMA
%token CTRL
%token TRY
%token CATCH
%token LAMBDA
%token LET
%token IN
%token EQUAL
%token PIPE
%token ARROW
%token VOID
%token QUNIT
%token U3
%token LEFT
%token RIGHT
%token GPHASE
%token <int> CONST
%token PI
%token EULER
%token PLUS
%token TIMES
%token MINUS
%token DIV
%token SIN
%token COS
%token TAN
%token ARCSIN
%token ARCCOS
%token ARCTAN
%token EXP
%token LN
%token SQRT
%token EOF

%nonassoc CATCH
%nonassoc IN
%left PIPE
%nonassoc ARROW
%left PLUS
%left MINUS
%left TIMES
%left DIV
%nonassoc SIN
%nonassoc COS
%nonassoc TAN
%nonassoc ARCSIN
%nonassoc ARCCOS
%nonassoc ARCTAN
%nonassoc EXP
%nonassoc LN
%nonassoc SQRT

%start <Syntax.expr> qunityfile

%%

qunityfile:
    | e = expr; EOF {e}
    ;

real:
    | PI {Pi}
    | EULER {Euler}
    | x = CONST {Const x}
    | MINUS; r = real {Negate r}
    | r1 = real; PLUS; r2 = real {Plus (r1, r2)}
    | r1 = real; TIMES; r2 = real {Times (r1, r2)}
    | r1 = real; MINUS; r2 = real {Plus (r1, Negate r2)}
    | r1 = real; DIV; r2 = real {Div (r1, r2)}
    | SIN; r = real {Sin r}
    | COS; r = real {Cos r}
    | TAN; r = real {Tan r}
    | ARCSIN; r = real {Arcsin r}
    | ARCCOS; r = real {Arccos r}
    | ARCTAN; r = real {Arctan r}
    | EXP; r = real {Exp r}
    | LN; r = real {Ln r}
    | SQRT; r = real {Sqrt r}
    | LPAREN; r = real; RPAREN {r}
    ;

exprpairlist:
    | e0 = expr; ARROW; e1 = expr; COMMA; l = exprpairlist {(e0, e1) :: l}
    | e0 = expr; ARROW; e1 = expr; RBRACKET {[(e0, e1)]}
    ;

exprnopipes:
    | NULL {Null}
    | x = VAR {Var x}
    | LPAREN; e0 = exprnopipes; COMMA; e1 = exprnopipes; RPAREN {Qpair (e0, e1)}
    | CTRL; LBRACE; t0 = exprtype; COMMA; t1 = exprtype; RBRACE; e = exprnopipes; LBRACKET;
        l = exprpairlist {Ctrl (e, t0, l, t1)}
    | TRY; e0 = exprnopipes; CATCH e1 = exprnopipes {Try (e0, e1)}
    | f = prog; e = exprnopipes; {Apply (f, e)}
    | LET; e0 = exprnopipes; LBRACE; t = exprtype; RBRACE; EQUAL; e1 = exprnopipes; IN;
        e2 = exprnopipes {Apply (Lambda (e0, t, e2), e1)}
    | LPAREN; e = exprnopipes; RPAREN {e}
    ;

expr:
    | NULL {Null}
    | x = VAR {Var x}
    | LPAREN; e0 = expr; COMMA; e1 = expr; RPAREN {Qpair (e0, e1)}
    | CTRL; LBRACE; t0 = exprtype; COMMA; t1 = exprtype; RBRACE; e = expr; LBRACKET;
        l = exprpairlist {Ctrl (e, t0, l, t1)}
    | TRY; e0 = expr; CATCH e1 = expr {Try (e0, e1)}
    | f = prog; e = exprnopipes; {Apply (f, e)}
    | e = expr; PIPE; f = prog {Apply (f, e)}
    | LET; e0 = expr; LBRACE; t = exprtype; RBRACE; EQUAL; e1 = expr; IN;
        e2 = expr {Apply (Lambda (e0, t, e2), e1)}
    | LPAREN; e = expr; RPAREN {e}
    ;

exprtype:
    | VOID {Void}
    | QUNIT {Qunit}
    | t0 = exprtype; PLUS; t1 = exprtype {SumType (t0, t1)}
    | t0 = exprtype; TIMES; t1 = exprtype {ProdType (t0, t1)}
    ;

prog:
    | U3; LBRACE; theta = real; COMMA; phi = real; COMMA; lambda = real; RBRACE {U3 (theta, phi, lambda)}
    | LEFT; LBRACE; t0 = exprtype; COMMA; t1 = exprtype; RBRACE {Left (t0, t1)}
    | RIGHT; LBRACE; t0 = exprtype; COMMA; t1 = exprtype; RBRACE {Right (t0, t1)}
    | LAMBDA; e0 = expr; LBRACE; t = exprtype; RBRACE; ARROW; e1 = expr {Lambda (e0, t, e1)}
    | GPHASE; LBRACE; t = exprtype; COMMA; r = real; RBRACE {Gphase (t, r)}
    | LPAREN; f = prog; RPAREN {f}
    ;
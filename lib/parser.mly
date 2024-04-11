%{
open Util
open Extended_syntax
%}

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token LANGLE
%token RANGLE
%token COMMA
%token SEMICOLON
%token EOF

%token NULL
%token <string> VAR
%token CTRL
%token TRY
%token CATCH
%token LAMBDA
%token LET
%token IN
%token OF
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
%token POW
%token SIN
%token COS
%token TAN
%token ARCSIN
%token ARCCOS
%token ARCTAN
%token EXP
%token LN
%token SQRT

%token DEF
%token DEFSTART
%token END
%token IF
%token THEN
%token ELSE
%token ENDIF
%token <string> XVAR

%nonassoc ARROW
%nonassoc CATCH
%nonassoc IN
%left PIPE
%nonassoc OF
%left PLUS
%left MINUS
%left TIMES
%left DIV
%right POW
%nonassoc SIN
%nonassoc COS
%nonassoc TAN
%nonassoc ARCSIN
%nonassoc ARCCOS
%nonassoc ARCTAN
%nonassoc EXP
%nonassoc LN
%nonassoc SQRT

%start <qunityfile> qunityfile
%start <qunityinteract> qunityinteract

%%

qunityfile:
    | e = xexpr; EOF {{dm = StringMap.empty; main = e}}
    | DEF; name = XVAR; LANGLE; l = argnames; DEFSTART; body = xexpr; END;
        qf = qunityfile {add_def name (l, body) qf}
    | DEF; name = XVAR; DEFSTART; body = xexpr; END;
        qf = qunityfile {add_def name ([], body) qf}
    ;

qunityinteract:
    | SEMICOLON; SEMICOLON; EOF {{dm = StringMap.empty; main = None}}
    | e = xexpr; SEMICOLON; SEMICOLON; EOF
        {{dm = StringMap.empty; main = Some e}}
    | DEF; name = XVAR; LANGLE; l = argnames; DEFSTART; body = xexpr; END;
        qi = qunityinteract {add_def_interact name (l, body) qi}
    | DEF; name = XVAR; DEFSTART; body = xexpr; END;
        qi = qunityinteract {add_def_interact name ([], body) qi}
    ;

xexpr:
    | NULL {Null}
    | x = VAR {Var x}
    | LPAREN; e0 = xexpr; COMMA; e1 = xexpr; RPAREN {Qpair (e0, e1)}
    | CTRL; LBRACE; t0 = xexpr; COMMA; t1 = xexpr; RBRACE; e = xexpr; LBRACKET;
        l = ctrlblock {Ctrl (e, t0, fst l, t1, snd l)}
    | TRY; e0 = xexpr; CATCH e1 = xexpr {Try (e0, e1)}
    | f = xexpr; OF; e = xexpr  {Apply (f, e)}
    | e = xexpr; PIPE; f = xexpr {Apply (f, e)}
    | LET; e0 = xexpr; LBRACE; t = xexpr; RBRACE; EQUAL; e1 = xexpr; IN;
        e2 = xexpr {Apply (Lambda (e0, t, e2), e1)}
    | VOID {Void}
    | QUNIT {Qunit}
    | t0 = xexpr; PLUS; t1 = xexpr {SumType (t0, t1)}
    | t0 = xexpr; TIMES; t1 = xexpr {ProdType (t0, t1)}
    | U3; LBRACE; theta = real; COMMA; phi = real; COMMA; lambda = real;
        RBRACE {U3 (theta, phi, lambda)}
    | LEFT; LBRACE; t0 = xexpr; COMMA; t1 = xexpr; RBRACE {Left (t0, t1)}
    | RIGHT; LBRACE; t0 = xexpr; COMMA; t1 = xexpr; RBRACE {Right (t0, t1)}
    | LAMBDA; e0 = xexpr; LBRACE; t = xexpr; RBRACE; ARROW;
        e1 = xexpr {Lambda (e0, t, e1)}
    | GPHASE; LBRACE; t = xexpr; COMMA; r = real; RBRACE {Gphase (t, r)}
    | LBRACKET; r = real; RBRACKET {XReal r}
    | name = XVAR {Invoke (name, [])}
    | name = XVAR; LANGLE; l = arglist {Invoke (name, l)}
    | IF; v0 = xexpr; EQUAL; v1 = xexpr; THEN; vtrue = xexpr;
        ELSE vfalse = xexpr; ENDIF {Ifeq (v0, v1, vtrue, vfalse)}
    | LPAREN; x = xexpr; RPAREN {x}
    ;

real:
    | PI {XPi}
    | EULER {XEuler}
    | x = CONST {XConst x}
    | x = XVAR; {XVar x}
    | MINUS; LPAREN; r = real; RPAREN {XNegate r}
    | r0 = real; PLUS; r1 = real {XPlus (r0, r1)}
    | r0 = real; TIMES; r1 = real {XTimes (r0, r1)}
    | r0 = real; MINUS; r1 = real {XPlus (r0, XNegate r1)}
    | r0 = real; DIV; r1 = real {XDiv (r0, r1)}
    | r0 = real; POW; r1 = real {XPow (r0, r1)}
    | SIN; r = real {XSin r}
    | COS; r = real {XCos r}
    | TAN; r = real {XTan r}
    | ARCSIN; r = real {XArcsin r}
    | ARCCOS; r = real {XArccos r}
    | ARCTAN; r = real {XArctan r}
    | EXP; r = real {XExp r}
    | LN; r = real {XLn r}
    | SQRT; r = real {XSqrt r}
    | LPAREN; r = real; RPAREN {r}
    ;

ctrlblock:
    | e0 = xexpr; ARROW; e1 = xexpr; SEMICOLON; l = ctrlblock
        {((e0, e1) :: fst l, snd l)}
    | e0 = xexpr; ARROW; e1 = xexpr; RBRACKET {([(e0, e1)], None)}
    | e0 = xexpr; ARROW; e1 = xexpr; SEMICOLON; ELSE; ARROW; e2 = xexpr RBRACKET
        {([(e0, e1)], Some e2)}
    ;

argnames:
    | name = XVAR; RANGLE {[name]}
    | name = XVAR; COMMA; l = argnames {name :: l}
    ;

arglist:
    | v = xexpr; RANGLE {[v]}
    | v = xexpr; COMMA; l = arglist {v :: l}
    ;

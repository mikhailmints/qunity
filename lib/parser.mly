%{
open Metaprogramming
%}

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token COMMA
%token SEMICOLON
%token VBAR
%token COLON
%token EOF

%token <string> QVAR
%token <string> TYPENAME
%token <string> TYPEVAR
%token <string> EXPRVAR
%token <string> PROGVAR
%token <string> REALVAR

%token NULL
%token CTRL
%token MATCH
%token PMATCH
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
%token UNIT
%token U3
%token RPHASE
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
%token LOG2
%token SQRT
%token CEIL
%token FLOOR
%token MOD

%token LEQ
%token GEQ
%token NEQ
%token LT
%token GT
%token AND
%token OR
%token NOT

%token DEF
%token DEFSTART
%token END
%token TYPE

%token IF
%token THEN
%token ELSE
%token ENDIF

%nonassoc NOT
%left AND
%left OR
%nonassoc ARROW
%nonassoc CATCH
%nonassoc IN
%left PIPE
%left PLUS
%left MINUS
%left TIMES
%left DIV
%left MOD
%right POW


%start <defmap * xexpr> qunityfile
%start <defmap> qunitylib

%%

qunityfile:
    | l = deflist; xe = xexpr; EOF {(l, xe)}
    ;

qunitylib:
    | l = deflist; EOF {l}
    ;

deflist:
    | {dm_empty}
    | l = deflist; def = definition {update_defmap_with_def l def}
    ;

definition:
    | TYPE; name = TYPENAME; psig = paramsig; DEFSTART; xt = xtype; END
        {Typedef (name, psig, xt)}
    | TYPE; name = TYPENAME; psig = paramsig; DEFSTART; cl = constructor_list;
        END {TypedefVariant (name, psig, cl)}
    | DEF; name = EXPRVAR; psig = paramsig; COLON; xt = xtype; DEFSTART;
        xe = xexpr; END {Exprdef (name, psig, xt, xe)}
    | DEF; name = PROGVAR; psig = paramsig; COLON; xt0t1 = xprogtype; DEFSTART;
        xf = xprog; END {Progdef (name, psig, fst xt0t1, snd xt0t1, xf)}
    | DEF; name = REALVAR; psig = paramsig; DEFSTART; xr = xreal; END
        {Realdef (name, psig, xr)}
    ;

paramsig:
    | {[]}
    | LBRACE; psig = paramsig_nonempty; RBRACE {psig}
    ;

paramsig_nonempty:
    | item = paramsig_item {[item]}
    | item = paramsig_item; COMMA; psig = paramsig_nonempty {item :: psig}
    ;

paramsig_item:
    | name = EXPRVAR; COLON; xt = xtype {(name, Expr xt)}
    | name = TYPEVAR {(name, Type)}
    | name = PROGVAR; COLON; xt0t1 = xprogtype
        {(name, Prog (fst xt0t1, snd xt0t1))}
    | name = REALVAR {(name, (fun (x : typed_sort) -> x) Real)}
    ;

arglist:
    | {[]}
    | LBRACE; l = arglist_nonempty; RBRACE {l}
    ;

arglist_nonempty:
    | item = arglist_item {[item]}
    | item = arglist_item; COMMA; l = arglist_nonempty {item :: l}
    ;

arglist_item:
    | xe = xexpr {Expr xe}
    | xt = xtype {Type xt}
    | xf = xprog {Prog xf}
    | xr = xreal {Real xr}
    ;

constructor_list:
    | VBAR; l = constructor_list1 {l}
    | l = constructor_list1 {l}
    ;

constructor_list1:
    | name = EXPRVAR {[(name, Qunit)]}
    | name = PROGVAR; OF; xt = xtype {[(name, xt)]}
    | name = EXPRVAR; VBAR; l = constructor_list1 {(name, Qunit) :: l}
    | name = PROGVAR; OF; xt = xtype; VBAR; l = constructor_list1
        {(name, xt) :: l}
    ;

xexpr:
    | NULL {Null}
    | x = QVAR {Var x}
    | LPAREN; xe0 = xexpr; COMMA; xe1 = xexpr; RPAREN {Qpair (xe0, xe1)}
    | CTRL; xe = xexpr; LBRACKET; l = ctrlblock {Ctrl (xe, fst l, snd l)}
    | MATCH; xe = xexpr; LBRACKET; l = ctrlblock {Match (xe, fst l, snd l)}
    | TRY; xe0 = xexpr; CATCH; xe1 = xexpr {Try (xe0, xe1)}
    | f = xprog; LPAREN; xe = xexpr; RPAREN {Apply (f, xe)}
    | f = xprog; LPAREN; xe0 = xexpr; COMMA; xe1 = xexpr; RPAREN
        {Apply (f, Qpair (xe0, xe1))}
    | xe = xexpr; PIPE; f = xprog {Apply (f, xe)} 
    | LET; xe0 = xexpr; EQUAL; xe1 = xexpr; IN; xe2 = xexpr
        {Apply (Lambda (xe0, xe2), xe1)}
    | name = EXPRVAR; l = arglist {ExprInvoke (name, l)}
    | LPAREN; xe = xexpr; RPAREN {xe}
    | IF; be = bexpr; THEN; xe0 = xexpr; ELSE; xe1 = xexpr; ENDIF
        {ExprIf (be, xe0, xe1)}
    ;

xtype:
    | VOID {Void}
    | UNIT {Qunit}
    | xt0 = xtype; TIMES; xt1 = xtype {ProdType (xt0, xt1)}
    | name = TYPEVAR {TypeVar name}
    | name = TYPENAME; l = arglist {TypeInvoke (name, l)}
    | LPAREN; xt = xtype; RPAREN {xt}
    | IF; be = bexpr; THEN; xt0 = xtype; ELSE; xt1 = xtype;
        ENDIF {TypeIf (be, xt0, xt1)}
    ;

xprogtype:
    | xt0 = xtype; ARROW; xt1 = xtype {(xt0, xt1)}
    | LPAREN; xt0t1 = xprogtype; RPAREN {xt0t1}
    ;

xprog:
    | U3; LBRACE; theta = xreal; COMMA; phi = xreal; COMMA; lambda = xreal;
        RBRACE {U3 (theta, phi, lambda)}
    | LAMBDA; xe0 = xexpr; ARROW; xe1 = xexpr {Lambda (xe0, xe1)}
    | RPHASE; LBRACE; xe = xexpr; COMMA; r0 = xreal; COMMA;
        r1 = xreal; RBRACE {Rphase (xe, r0, r1)}
    | GPHASE; LBRACE; r = xreal; RBRACE {Rphase (Var "x", r, r)}
    | PMATCH; LBRACKET; l = pmatchblock {Pmatch l}
    | name = PROGVAR; l = arglist {ProgInvoke (name, l)}
    | LPAREN; xf = xprog; RPAREN {xf}
    | IF; be = bexpr; THEN; xf0 = xprog; ELSE; xf1 = xprog;
        ENDIF {ProgIf (be, xf0, xf1)}
    ;

xreal:
    | PI {Pi}
    | EULER {Euler}
    | x = CONST {Const x}
    | MINUS; r = xreal {Negate r}
    | r0 = xreal; PLUS; r1 = xreal {Plus (r0, r1)}
    | r0 = xreal; TIMES; r1 = xreal {Times (r0, r1)}
    | r0 = xreal; MINUS; r1 = xreal {Plus (r0, Negate r1)}
    | r0 = xreal; DIV; r1 = xreal {Div (r0, r1)}
    | r0 = xreal; POW; r1 = xreal {Pow (r0, r1)}
    | r0 = xreal; MOD; r1 = xreal {Mod (r0, r1)}
    | SIN; LPAREN; r = xreal; RPAREN {Sin r}
    | COS; LPAREN; r = xreal; RPAREN {Cos r}
    | TAN; LPAREN; r = xreal; RPAREN {Tan r}
    | ARCSIN; LPAREN; r = xreal; RPAREN {Arcsin r}
    | ARCCOS; LPAREN; r = xreal; RPAREN {Arccos r}
    | ARCTAN; LPAREN; r = xreal; RPAREN {Arctan r}
    | EXP; LPAREN; r = xreal; RPAREN {Exp r}
    | LN; LPAREN; r = xreal; RPAREN {Ln r}
    | LOG2; LPAREN; r = xreal; RPAREN {Log2 r}
    | SQRT; LPAREN; r = xreal; RPAREN {Sqrt r}
    | CEIL; LPAREN; r = xreal; RPAREN {Ceil r}
    | FLOOR; LPAREN; r = xreal; RPAREN {Floor r}
    | name = REALVAR; l = arglist {RealInvoke (name, l)}
    | LPAREN; r = xreal; RPAREN {r}
    | IF; be = bexpr; THEN; r0 = xreal; ELSE; r1 = xreal; ENDIF
        {RealIf (be, r0, r1)}
    ;

bexpr:
    | NOT; be = bexpr {Not be}
    | be0 = bexpr; AND; be1 = bexpr {And (be0, be1)}
    | be0 = bexpr; OR; be1 = bexpr {Or (be0, be1)}
    | be0 = xreal; cmp = comparison; be1 = xreal {Cmp (cmp, be0, be1)}
    | LPAREN; be = bexpr; RPAREN {be}
    ;

comparison:
    | EQUAL {Eq}
    | NEQ {Neq}
    | LEQ {Leq}
    | LT {Lt}
    | GEQ {Geq}
    | GT {Gt}
    ;

ctrlblock:
    | e0 = xexpr; ARROW; e1 = xexpr; SEMICOLON; l = ctrlblock
        {((e0, e1) :: fst l, snd l)}
    | e0 = xexpr; ARROW; e1 = xexpr; RBRACKET {([(e0, e1)], None)}
    | RBRACKET {([], None)}
    | e0 = xexpr; ARROW; e1 = xexpr; SEMICOLON; ELSE; ARROW; e2 = xexpr;
        RBRACKET {([(e0, e1)], Some e2)}
    | e0 = xexpr; ARROW; e1 = xexpr; SEMICOLON; ELSE; ARROW; e2 = xexpr;
        SEMICOLON; RBRACKET {([(e0, e1)], Some e2)}
    ;

pmatchblock:
    | e0 = xexpr; ARROW; e1 = xexpr; SEMICOLON; l = pmatchblock {(e0, e1) :: l}
    | e0 = xexpr; ARROW; e1 = xexpr; RBRACKET {[(e0, e1)]}
    | RBRACKET {[]}
    ;

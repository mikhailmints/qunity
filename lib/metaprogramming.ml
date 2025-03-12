open Util
open Reals
open Syntax
open Typechecking

let debug_mode = ref false

type sort = Expr | Type | Prog | Real
type comparison = Eq | Neq | Lt | Gt | Leq | Geq

type typed_sort = Expr of xtype | Type | Prog of (xtype * xtype) | Real
and gexpr = Expr of xexpr | Type of xtype | Prog of xprog | Real of xreal

and bexpr =
  | Not of bexpr
  | And of (bexpr * bexpr)
  | Or of (bexpr * bexpr)
  | Cmp of (comparison * xreal * xreal)

and xreal =
  | Pi
  | Euler
  | Const of int
  | Negate of xreal
  | Plus of (xreal * xreal)
  | Times of (xreal * xreal)
  | Div of (xreal * xreal)
  | Pow of (xreal * xreal)
  | Mod of (xreal * xreal)
  | Sin of xreal
  | Cos of xreal
  | Tan of xreal
  | Arcsin of xreal
  | Arccos of xreal
  | Arctan of xreal
  | Exp of xreal
  | Ln of xreal
  | Log2 of xreal
  | Sqrt of xreal
  | Ceil of xreal
  | Floor of xreal
  | RealInvoke of (string * gexpr list)
  | RealIf of (bexpr * xreal * xreal)
  | RealFail

and paramsig = (string * typed_sort) list

and definition =
  | Typedef of (string * paramsig * xtype)
  | TypedefVariant of (string * paramsig * (string * xtype) list)
  | Exprdef of (string * paramsig * xtype * xexpr)
  | Progdef of (string * paramsig * xtype * xtype * xprog)
  | Realdef of (string * paramsig * xreal)

and xtype =
  | Void
  | Qunit
  | ProdType of (xtype * xtype)
  | TypeVar of string
  | TypeInvoke of (string * gexpr list)
  | TypeIf of (bexpr * xtype * xtype)
  | TypeFail

and xexpr =
  | Null
  | Var of string
  | Qpair of (xexpr * xexpr)
  | Ctrl of (xexpr * (xexpr * xexpr) list * xexpr option)
  | Match of (xexpr * (xexpr * xexpr) list * xexpr option)
  | Try of (xexpr * xexpr)
  | Apply of (xprog * xexpr)
  | ExprInvoke of string * gexpr list
  | ExprIf of (bexpr * xexpr * xexpr)
  | ExprFail

and xprog =
  | U3 of (xreal * xreal * xreal)
  | Lambda of (xexpr * xexpr)
  | Rphase of (xexpr * xreal * xreal)
  | Pmatch of (xexpr * xexpr) list
  | ProgInvoke of string * gexpr list
  | ProgIf of (bexpr * xprog * xprog)
  | ProgFail

type xcontext = xtype StringMap.t

type defmap = {
  defs : definition StringMap.t;
  constructors : (string * paramsig * xtype) StringMap.t;
  expr_vars : (expr * xtype * xcontext) StringMap.t;
  type_vars : xtype StringMap.t;
      (* While expression, program, and real variables are reduced to base
          Qunity syntax when stored here, type variables are not and are stored
          as xtype, only being fully evaluated at the end. This makes it
          possible to do type inference. *)
  prog_vars : (prog * xtype * xtype) StringMap.t;
  real_vars : real StringMap.t;
}

let name_of_def (def : definition) : string =
  match def with
  | Typedef (name, _, _)
  | TypedefVariant (name, _, _)
  | Exprdef (name, _, _, _)
  | Progdef (name, _, _, _, _)
  | Realdef (name, _, _) ->
      name

let rec string_of_bexpr (be : bexpr) : string =
  match be with
  | Not be0 -> Printf.sprintf "!(%s)" (string_of_bexpr be0)
  | And (be0, be1) ->
      Printf.sprintf "(%s) && (%s)" (string_of_bexpr be0) (string_of_bexpr be1)
  | Or (be0, be1) ->
      Printf.sprintf "(%s) || (%s)" (string_of_bexpr be0) (string_of_bexpr be1)
  | Cmp (cmp, xr0, xr1) ->
      Printf.sprintf "(%s) %s (%s)" (string_of_xreal xr0)
        (match cmp with
        | Eq -> "="
        | Neq -> "!="
        | Lt -> "<"
        | Gt -> ">"
        | Leq -> "<="
        | Geq -> ">=")
        (string_of_xreal xr1)

and string_of_xreal (xr : xreal) : string =
  match xr with
  | Pi -> "pi"
  | Euler -> "euler"
  | Const x -> string_of_int x
  | Negate xr0 -> Printf.sprintf "-(%s)" (string_of_xreal xr0)
  | Plus (xr0, xr1) ->
      Printf.sprintf "(%s) + (%s)" (string_of_xreal xr0) (string_of_xreal xr1)
  | Times (xr0, xr1) ->
      Printf.sprintf "(%s) * (%s)" (string_of_xreal xr0) (string_of_xreal xr1)
  | Div (xr0, xr1) ->
      Printf.sprintf "(%s) / (%s)" (string_of_xreal xr0) (string_of_xreal xr1)
  | Pow (xr0, xr1) ->
      Printf.sprintf "(%s) ^ (%s)" (string_of_xreal xr0) (string_of_xreal xr1)
  | Mod (xr0, xr1) ->
      Printf.sprintf "(%s) %% (%s)" (string_of_xreal xr0) (string_of_xreal xr1)
  | Sin xr0 -> Printf.sprintf "sin(%s)" (string_of_xreal xr0)
  | Cos xr0 -> Printf.sprintf "cos(%s)" (string_of_xreal xr0)
  | Tan xr0 -> Printf.sprintf "tan(%s)" (string_of_xreal xr0)
  | Arcsin xr0 -> Printf.sprintf "arcsin(%s)" (string_of_xreal xr0)
  | Arccos xr0 -> Printf.sprintf "arccos(%s)" (string_of_xreal xr0)
  | Arctan xr0 -> Printf.sprintf "arctan(%s)" (string_of_xreal xr0)
  | Exp xr0 -> Printf.sprintf "exp(%s)" (string_of_xreal xr0)
  | Ln xr0 -> Printf.sprintf "ln(%s)" (string_of_xreal xr0)
  | Log2 xr0 -> Printf.sprintf "log2(%s)" (string_of_xreal xr0)
  | Sqrt xr0 -> Printf.sprintf "sqrt(%s)" (string_of_xreal xr0)
  | Ceil xr0 -> Printf.sprintf "ceil(%s)" (string_of_xreal xr0)
  | Floor xr0 -> Printf.sprintf "floor(%s)" (string_of_xreal xr0)
  | RealInvoke (name, params) -> begin
      match params with
      | [] -> name
      | _ ->
          let params_str_list =
            List.map
              (fun (ge : gexpr) ->
                match ge with
                | Expr _ -> "[expr]"
                | Type _ -> "[type]"
                | Prog _ -> "[prog]"
                | Real xr -> Printf.sprintf "%s" (string_of_xreal xr))
              params
          in
            Printf.sprintf "%s {%s}" name (String.concat ", " params_str_list)
    end
  | RealIf (be, xr0, xr1) ->
      Printf.sprintf "if %s then %s else %s" (string_of_bexpr be)
        (string_of_xreal xr0) (string_of_xreal xr1)
  | RealFail -> "fail"

let rec string_of_xtype (xt : xtype) : string =
  match xt with
  | Void -> "Void"
  | Qunit -> "Unit"
  | ProdType (xt0, xt1) ->
      Printf.sprintf "%s * %s" (string_of_xtype xt0) (string_of_xtype xt1)
  | TypeVar name -> name
  | TypeInvoke (name, params) -> begin
      match params with
      | [] -> name
      | _ ->
          let params_str_list =
            List.map
              (fun (ge : gexpr) ->
                match ge with
                | Expr _ -> "[expr]"
                | Type xt' -> string_of_xtype xt'
                | Prog _ -> "[prog]"
                | Real xr -> string_of_xreal xr)
              params
          in
            Printf.sprintf "%s {%s}" name (String.concat ", " params_str_list)
    end
  | TypeIf (be, xt0, xt1) ->
      Printf.sprintf "if %s then %s else %s" (string_of_bexpr be)
        (string_of_xtype xt0) (string_of_xtype xt1)
  | TypeFail -> "fail"

and string_of_xcontext (d : xcontext) : string =
  StringMap.bindings d
  |> List.map (fun (x, xt) -> Printf.sprintf "%s : %s" x (string_of_xtype xt))
  |> String.concat ", " |> Printf.sprintf "{%s}"

let string_of_xtype_opt (xt : xtype option) : string =
  match xt with
  | Some xt -> string_of_xtype xt
  | None -> "[?]"

let rec string_of_xexpr (xe : xexpr) : string =
  match xe with
  | Null -> "()"
  | Var x -> x
  | Qpair (xe0, xe1) ->
      Printf.sprintf "(%s, %s)" (string_of_xexpr xe0) (string_of_xexpr xe1)
  | Ctrl (xe0, l, xelseopt) ->
      let l =
        match xelseopt with
        | None -> l
        | Some xelse -> l @ [(Var "else", xelse)]
        (* This is fine for printing purposes *)
      in
        Printf.sprintf "ctrl %s [%s]" (string_of_xexpr xe0)
          (List.fold_left
             (fun s1 s2 -> if s1 = "" then s2 else s1 ^ "; " ^ s2)
             ""
             (List.map
                (fun (xej, xej') ->
                  Printf.sprintf "(%s) -> (%s)" (string_of_xexpr xej)
                    (string_of_xexpr xej'))
                l))
  | Match (xe0, l, xelseopt) ->
      let l =
        match xelseopt with
        | None -> l
        | Some xelse -> l @ [(Var "else", xelse)]
      in
        Printf.sprintf "match %s [%s]" (string_of_xexpr xe0)
          (List.fold_left
             (fun s1 s2 -> if s1 = "" then s2 else s1 ^ "; " ^ s2)
             ""
             (List.map
                (fun (xej, xej') ->
                  Printf.sprintf "(%s) -> (%s)" (string_of_xexpr xej)
                    (string_of_xexpr xej'))
                l))
  | Try (xe0, xe1) ->
      Printf.sprintf "try %s catch %s" (string_of_xexpr xe0)
        (string_of_xexpr xe1)
  | Apply (xf, xe') ->
      Printf.sprintf "(%s) (%s)" (string_of_xprog xf) (string_of_xexpr xe')
  | ExprInvoke (name, params) ->
      let params_str_list =
        List.map
          (fun (ge : gexpr) ->
            match ge with
            | Expr xe -> string_of_xexpr xe
            | Type xt -> string_of_xtype xt
            | Prog xf -> string_of_xprog xf
            | Real xr -> string_of_xreal xr)
          params
      in
        if params_str_list = [] then
          name
        else
          Printf.sprintf "%s {%s}" name (String.concat ", " params_str_list)
  | ExprIf (be, xe0, xe1) ->
      Printf.sprintf "if %s then %s else %s" (string_of_bexpr be)
        (string_of_xexpr xe0) (string_of_xexpr xe1)
  | ExprFail -> "fail"

and string_of_xprog (xf : xprog) : string =
  match xf with
  | U3 (theta, phi, lambda) ->
      Printf.sprintf "u3 {%s, %s, %s}" (string_of_xreal theta)
        (string_of_xreal phi) (string_of_xreal lambda)
  | Lambda (xe, xe') ->
      Printf.sprintf "lambda (%s) -> (%s)" (string_of_xexpr xe)
        (string_of_xexpr xe')
  | Rphase (xe, r0, r1) ->
      Printf.sprintf "rphase {%s, %s, %s}" (string_of_xexpr xe)
        (string_of_xreal r0) (string_of_xreal r1)
  | Pmatch l ->
      Printf.sprintf "pmatch [%s]"
        (List.fold_left
           (fun s1 s2 -> if s1 = "" then s2 else s1 ^ "; " ^ s2)
           ""
           (List.map
              (fun (xe0, xe1) ->
                Printf.sprintf "(%s) -> (%s)" (string_of_xexpr xe0)
                  (string_of_xexpr xe1))
              l))
  | ProgInvoke (name, params) ->
      let params_str_list =
        List.map
          (fun (ge : gexpr) ->
            match ge with
            | Expr xe -> string_of_xexpr xe
            | Type xt -> string_of_xtype xt
            | Prog xf -> string_of_xprog xf
            | Real xr -> string_of_xreal xr)
          params
      in
        if params_str_list = [] then
          name
        else
          Printf.sprintf "%s {%s}" name (String.concat ", " params_str_list)
  | ProgIf (be, xf0, xf1) ->
      Printf.sprintf "if %s then %s else %s" (string_of_bexpr be)
        (string_of_xprog xf0) (string_of_xprog xf1)
  | ProgFail -> "fail"

let type_mismatch_message_expr (expected : xtype option) (actual : xtype) :
    string =
  Printf.sprintf
    "Expected expression of type %s, but got expression of type %s"
    (string_of_xtype_opt expected)
    (string_of_xtype actual)

let type_mismatch_message_prog (expected_in : xtype)
    (expected_out : xtype option) (actual_in : xtype) (actual_out : xtype) :
    string =
  Printf.sprintf
    "Expected program of type %s -> %s, but got program of type %s -> %s"
    (string_of_xtype expected_in)
    (string_of_xtype_opt expected_out)
    (string_of_xtype actual_in)
    (string_of_xtype actual_out)

let dm_empty : defmap =
  {
    defs = StringMap.empty;
    constructors = StringMap.empty;
    expr_vars = StringMap.empty;
    type_vars = StringMap.empty;
    prog_vars = StringMap.empty;
    real_vars = StringMap.empty;
  }

let string_of_defmap (dm : defmap) : string =
  let expr_vars_str =
    StringMap.bindings dm.expr_vars
    |> List.map (fun (name, (e, xt, fv)) ->
           Printf.sprintf "%s = %s : %s fv = {%s}" name (string_of_expr e)
             (string_of_xtype xt) (string_of_xcontext fv))
    |> String.concat ", "
  in
  let type_vars_str =
    StringMap.bindings dm.type_vars
    |> List.map (fun (name, xt) ->
           Printf.sprintf "%s = %s" name (string_of_xtype xt))
    |> String.concat ", "
  in
  let prog_vars_str =
    StringMap.bindings dm.prog_vars
    |> List.map (fun (name, (f, xt0, xt1)) ->
           Printf.sprintf "%s = %s : %s -> %s" name (string_of_prog f)
             (string_of_xtype xt0) (string_of_xtype xt1))
    |> String.concat ", "
  in
  let real_vars_str =
    StringMap.bindings dm.real_vars
    |> List.map (fun (name, r) ->
           Printf.sprintf "%s = %s" name (string_of_real r))
    |> String.concat ", "
  in
    Printf.sprintf
      "{ expr_vars = [%s]; type_vars = [%s]; prog_vars = [%s]; real_vars = \
       [%s] }"
      expr_vars_str type_vars_str prog_vars_str real_vars_str

let string_of_gexpr (ge : gexpr) : string =
  match ge with
  | Expr xe -> string_of_xexpr xe
  | Type xt -> string_of_xtype xt
  | Prog xf -> string_of_xprog xf
  | Real xr -> string_of_xreal xr

let sort_of_gexpr (ge : gexpr) : sort =
  match ge with
  | Expr _ -> Expr
  | Type _ -> Type
  | Prog _ -> Prog
  | Real _ -> Real

let sort_of_typed_sort (ts : typed_sort) : sort =
  match ts with
  | Expr _ -> Expr
  | Type -> Type
  | Prog _ -> Prog
  | Real -> Real

let rec xreal_of_real (r : real) : xreal =
  match r with
  | Pi -> Pi
  | Euler -> Euler
  | Const x -> Const x
  | Negate r' -> Negate (xreal_of_real r')
  | Plus (r1, r2) -> Plus (xreal_of_real r1, xreal_of_real r2)
  | Times (r1, r2) -> Times (xreal_of_real r1, xreal_of_real r2)
  | Div (r1, r2) -> Div (xreal_of_real r1, xreal_of_real r2)
  | Pow (r1, r2) -> Pow (xreal_of_real r1, xreal_of_real r2)
  | Mod (r1, r2) -> Mod (xreal_of_real r1, xreal_of_real r2)
  | Sin r' -> Sin (xreal_of_real r')
  | Cos r' -> Cos (xreal_of_real r')
  | Tan r' -> Tan (xreal_of_real r')
  | Arcsin r' -> Arcsin (xreal_of_real r')
  | Arccos r' -> Arccos (xreal_of_real r')
  | Arctan r' -> Arctan (xreal_of_real r')
  | Exp r' -> Exp (xreal_of_real r')
  | Ln r' -> Ln (xreal_of_real r')
  | Log2 r' -> Log2 (xreal_of_real r')
  | Sqrt r' -> Sqrt (xreal_of_real r')
  | Ceil r' -> Ceil (xreal_of_real r')
  | Floor r' -> Floor (xreal_of_real r')

let rec update_defmap_with_def (dm : defmap) (def : definition) : defmap =
  let dm = { dm with defs = StringMap.add (name_of_def def) def dm.defs } in
    match def with
    | TypedefVariant (typename, psig, l) -> begin
        List.fold_left
          (fun dm (name, xt) ->
            {
              dm with
              constructors =
                StringMap.add name (typename, psig, xt) dm.constructors;
            })
          dm l
      end
    | _ -> dm

and combine_defmaps (dm : defmap) (dm_new : defmap) : defmap =
  {
    defs = StringMap.union (fun _ _ b -> Some b) dm.defs dm_new.defs;
    constructors =
      StringMap.union (fun _ _ b -> Some b) dm.constructors dm_new.constructors;
    expr_vars =
      StringMap.union (fun _ _ b -> Some b) dm.expr_vars dm_new.expr_vars;
    type_vars =
      StringMap.union (fun _ _ b -> Some b) dm.type_vars dm_new.type_vars;
    prog_vars =
      StringMap.union (fun _ _ b -> Some b) dm.prog_vars dm_new.prog_vars;
    real_vars =
      StringMap.union (fun _ _ b -> Some b) dm.real_vars dm_new.real_vars;
  }

and update_defmap_with_params (dm : defmap) (d : xcontext) (psig : paramsig)
    (params : gexpr list) : defmap optionE =
  if
    List.length psig <> List.length params
    || List.map sort_of_gexpr params
       <> List.map (fun x -> x |> snd |> sort_of_typed_sort) psig
  then
    NoneE "Parameters do not match signature"
  else
    let dm_init = dm in
    let result =
      List.fold_left
        begin
          fun (dm : defmap optionE)
              (((name, ts), ge) : (string * typed_sort) * gexpr) ->
            match dm with
            | SomeE dm -> begin
                match (ge, ts) with
                | Expr xe, Expr xt -> begin
                    match xtype_reduce dm xt with
                    | SomeE xt' -> begin
                        match xexpr_eval dm_init d (Some xt') xe with
                        | SomeE (e, _, fv) ->
                            SomeE
                              {
                                dm with
                                expr_vars =
                                  StringMap.add name (e, xt', fv) dm.expr_vars;
                              }
                        | NoneE err -> NoneE err
                      end
                    | NoneE err -> NoneE err
                  end
                | Type xt, Type -> begin
                    match xtype_reduce dm xt with
                    | SomeE xt' ->
                        SomeE
                          {
                            dm with
                            type_vars = StringMap.add name xt' dm.type_vars;
                          }
                    | NoneE err -> NoneE err
                  end
                | Prog xf, Prog (in_type, out_type) -> begin
                    match
                      (xtype_reduce dm in_type, xtype_reduce dm out_type)
                    with
                    | SomeE in_type', SomeE out_type' -> begin
                        match
                          xprog_eval dm_init (Some in_type') (Some out_type')
                            xf
                        with
                        | SomeE (f, _, _) ->
                            SomeE
                              {
                                dm with
                                prog_vars =
                                  StringMap.add name (f, in_type', out_type')
                                    dm.prog_vars;
                              }
                        | NoneE err -> NoneE err
                      end
                    | NoneE err, _
                    | _, NoneE err ->
                        NoneE err
                  end
                | Real xr, Real -> begin
                    match xreal_eval dm_init xr with
                    | SomeE r ->
                        SomeE
                          {
                            dm with
                            real_vars = StringMap.add name r dm.real_vars;
                          }
                    | NoneE err -> NoneE err
                  end
                | _ -> NoneE "Parameters do not match signature"
              end
            | NoneE err -> NoneE err
        end
        (SomeE dm) (List.combine psig params)
    in
      if !debug_mode then
        Printf.printf
          "update_defmap_with_params:\n\
           dm_init = %s\n\
           params = %s\n\
           result = %s\n\n"
          (string_of_defmap dm_init)
          (string_of_list string_of_gexpr params)
          (match result with
          | SomeE result -> string_of_defmap result
          | NoneE err -> err);
      result

and xtype_reduce (dm : defmap) (xt : xtype) : xtype optionE =
  if !debug_mode then
    Printf.printf "xtype_reduce:\ndm = %s\nxt = %s\n\n" (string_of_defmap dm)
      (string_of_xtype xt);
  match xt with
  | Void -> SomeE Void
  | Qunit -> SomeE Qunit
  | ProdType (xt0, xt1) -> begin
      match (xtype_reduce dm xt0, xtype_reduce dm xt1) with
      | SomeE xt0', SomeE xt1' -> SomeE (ProdType (xt0', xt1'))
      | NoneE err, _
      | _, NoneE err ->
          NoneE err
    end
  | TypeVar name -> begin
      match StringMap.find_opt name dm.type_vars with
      | Some xt' ->
          if xt = xt' then
            NoneE "Infinite loop detected in type reduction"
          else
            xtype_reduce dm xt'
      | None -> failwith (Printf.sprintf "Type variable %s not found" name)
    end
  | TypeInvoke (name, params) -> begin
      match StringMap.find_opt name dm.defs with
      | Some (Typedef (_, psig, body)) -> begin
          match update_defmap_with_params dm StringMap.empty psig params with
          | SomeE dm' -> xtype_reduce dm' body
          | NoneE err -> NoneE err
        end
      | Some _ -> begin
          let l =
            List.map
              begin
                fun (ge : gexpr) : gexpr optionE ->
                  match ge with
                  | Expr _ -> NoneE "Type parameters cannot be expressions"
                  | Type xt -> begin
                      match xtype_reduce dm xt with
                      | SomeE xt' -> SomeE (Type xt')
                      | NoneE err -> NoneE err
                    end
                  | Prog _ -> NoneE "Type parameters cannot be programs"
                  | Real xr -> begin
                      match xreal_eval dm xr with
                      | SomeE r -> SomeE (Real (xreal_of_real r))
                      | NoneE err -> NoneE err
                    end
              end
              params
          in
            match all_or_nothing_optionE l with
            | SomeE params' -> SomeE (TypeInvoke (name, params'))
            | NoneE err -> NoneE err
        end
      | None -> NoneE (Printf.sprintf "Type %s not found" name)
    end
  | TypeIf (be, xt0, xt1) -> begin
      match bexpr_eval dm be with
      | SomeE true -> xtype_reduce dm xt0
      | SomeE false -> xtype_reduce dm xt1
      | NoneE err -> NoneE err
    end
  | TypeFail -> SomeE TypeFail

and xtype_eval (dm : defmap) (xt : xtype) : exprtype optionE =
  if !debug_mode then
    Printf.printf "xtype_eval:\ndm = %s\nxt = %s\n\n" (string_of_defmap dm)
      (string_of_xtype xt);
  match xt with
  | Void -> SomeE Void
  | Qunit -> SomeE Qunit
  | ProdType (xt0, xt1) -> begin
      match (xtype_eval dm xt0, xtype_eval dm xt1) with
      | SomeE t0, SomeE t1 -> SomeE (ProdType (t0, t1))
      | NoneE err, _
      | _, NoneE err ->
          NoneE err
    end
  | TypeVar name -> begin
      match StringMap.find_opt name dm.type_vars with
      | Some xt' -> xtype_eval dm xt'
      | None -> failwith (Printf.sprintf "Type variable %s not found" name)
    end
  | TypeInvoke (name, params) -> begin
      match StringMap.find_opt name dm.defs with
      | Some (Typedef (_, psig, body)) -> begin
          match update_defmap_with_params dm StringMap.empty psig params with
          | SomeE dm' -> xtype_eval dm' body
          | NoneE err -> NoneE err
        end
      | Some (TypedefVariant (_, psig, constructors)) -> begin
          match update_defmap_with_params dm StringMap.empty psig params with
          | SomeE dm' -> begin
              List.fold_right
                begin
                  fun (xt : xtype) (t : exprtype optionE) ->
                    begin
                      match (xtype_eval dm' xt, t) with
                      | SomeE t', SomeE Void -> SomeE t'
                      | SomeE t', SomeE t -> SomeE (SumType (t', t))
                      | NoneE err, _
                      | _, NoneE err ->
                          NoneE err
                    end
                end
                (List.map snd constructors)
                (SomeE Void)
              (* TODO does this cause problems if one of the constructors is void *)
            end
          | NoneE err -> NoneE err
        end
      | _ -> NoneE (Printf.sprintf "Name %s not found" name)
    end
  | TypeIf (be, xt0, xt1) -> begin
      match bexpr_eval dm be with
      | SomeE true -> xtype_eval dm xt0
      | SomeE false -> xtype_eval dm xt1
      | NoneE err -> NoneE err
    end
  | TypeFail -> NoneE "Failure triggered"

and bexpr_eval (dm : defmap) (be : bexpr) : bool optionE =
  match be with
  | Not be' -> begin
      match bexpr_eval dm be' with
      | SomeE b -> SomeE (not b)
      | NoneE err -> NoneE err
    end
  | And (be0, be1) -> begin
      match (bexpr_eval dm be0, bexpr_eval dm be1) with
      | SomeE b0, SomeE b1 -> SomeE (b0 && b1)
      | NoneE err, _
      | _, NoneE err ->
          NoneE err
    end
  | Or (be0, be1) -> begin
      match (bexpr_eval dm be0, bexpr_eval dm be1) with
      | SomeE b0, SomeE b1 -> SomeE (b0 || b1)
      | NoneE err, _
      | _, NoneE err ->
          NoneE err
    end
  | Cmp (cmp, xr0, xr1) -> begin
      match (xreal_eval dm xr0, xreal_eval dm xr1) with
      | SomeE r0, SomeE r1 -> begin
          match cmp with
          | Eq -> SomeE (real_equal r0 r1)
          | Neq -> SomeE (not (real_equal r0 r1))
          | Lt -> SomeE (real_lt r0 r1)
          | Gt -> SomeE (real_gt r0 r1)
          | Leq -> SomeE (real_le r0 r1)
          | Geq -> SomeE (real_ge r0 r1)
        end
      | NoneE err, _
      | _, NoneE err ->
          NoneE err
    end

and xreal_eval (dm : defmap) (xr : xreal) : real optionE =
  match xr with
  | Pi -> SomeE Pi
  | Euler -> SomeE Euler
  | Const x -> SomeE (Const x)
  | Negate xr' -> begin
      match xreal_eval dm xr' with
      | SomeE (Const x) -> SomeE (Const (-x))
      | SomeE r -> SomeE (Negate r)
      | NoneE err -> NoneE err
    end
  | Plus (xr1, xr2) -> begin
      match (xreal_eval dm xr1, xreal_eval dm xr2) with
      | SomeE (Const x1), SomeE (Const x2) -> SomeE (Const (x1 + x2))
      | SomeE r1, SomeE r2 -> SomeE (Plus (r1, r2))
      | NoneE err, _
      | _, NoneE err ->
          NoneE err
    end
  | Times (xr1, xr2) -> begin
      match (xreal_eval dm xr1, xreal_eval dm xr2) with
      | SomeE (Const x1), SomeE (Const x2) -> SomeE (Const (x1 * x2))
      | SomeE r1, SomeE r2 -> SomeE (Times (r1, r2))
      | NoneE err, _
      | _, NoneE err ->
          NoneE err
    end
  | Div (xr1, xr2) -> begin
      match (xreal_eval dm xr1, xreal_eval dm xr2) with
      | SomeE r1, SomeE r2 -> SomeE (Div (r1, r2))
      | NoneE err, _
      | _, NoneE err ->
          NoneE err
    end
  | Pow (xr1, xr2) -> begin
      match (xreal_eval dm xr1, xreal_eval dm xr2) with
      | SomeE r1, SomeE r2 -> SomeE (Pow (r1, r2))
      | NoneE err, _
      | _, NoneE err ->
          NoneE err
    end
  | Mod (xr1, xr2) -> begin
      match (xreal_eval dm xr1, xreal_eval dm xr2) with
      | SomeE r1, SomeE r2 -> SomeE (Mod (r1, r2))
      | NoneE err, _
      | _, NoneE err ->
          NoneE err
    end
  | Sin xr' -> begin
      match xreal_eval dm xr' with
      | SomeE r -> SomeE (Sin r)
      | NoneE err -> NoneE err
    end
  | Cos xr' -> begin
      match xreal_eval dm xr' with
      | SomeE r -> SomeE (Cos r)
      | NoneE err -> NoneE err
    end
  | Tan xr' -> begin
      match xreal_eval dm xr' with
      | SomeE r -> SomeE (Tan r)
      | NoneE err -> NoneE err
    end
  | Arcsin xr' -> begin
      match xreal_eval dm xr' with
      | SomeE r -> SomeE (Arcsin r)
      | NoneE err -> NoneE err
    end
  | Arccos xr' -> begin
      match xreal_eval dm xr' with
      | SomeE r -> SomeE (Arccos r)
      | NoneE err -> NoneE err
    end
  | Arctan xr' -> begin
      match xreal_eval dm xr' with
      | SomeE r -> SomeE (Arctan r)
      | NoneE err -> NoneE err
    end
  | Exp xr' -> begin
      match xreal_eval dm xr' with
      | SomeE r -> SomeE (Exp r)
      | NoneE err -> NoneE err
    end
  | Ln xr' -> begin
      match xreal_eval dm xr' with
      | SomeE r -> SomeE (Ln r)
      | NoneE err -> NoneE err
    end
  | Log2 xr' -> begin
      match xreal_eval dm xr' with
      | SomeE r -> SomeE (Log2 r)
      | NoneE err -> NoneE err
    end
  | Sqrt xr' -> begin
      match xreal_eval dm xr' with
      | SomeE r -> SomeE (Sqrt r)
      | NoneE err -> NoneE err
    end
  | Ceil xr' -> begin
      match xreal_eval dm xr' with
      | SomeE r -> SomeE (Ceil r)
      | NoneE err -> NoneE err
    end
  | Floor xr' -> begin
      match xreal_eval dm xr' with
      | SomeE r -> SomeE (Floor r)
      | NoneE err -> NoneE err
    end
  | RealInvoke (name, params) -> begin
      match
        (StringMap.find_opt name dm.real_vars, StringMap.find_opt name dm.defs)
      with
      | Some r, _ -> SomeE r
      | _, Some (Realdef (_, psig, body)) -> begin
          match update_defmap_with_params dm StringMap.empty psig params with
          | SomeE dm' -> xreal_eval dm' body
          | NoneE err -> NoneE err
        end
      | _ -> NoneE (Printf.sprintf "Name %s not found" name)
    end
  | RealIf (be, xr0, xr1) -> begin
      match bexpr_eval dm be with
      | SomeE true -> xreal_eval dm xr0
      | SomeE false -> xreal_eval dm xr1
      | NoneE err -> NoneE err
    end
  | RealFail -> NoneE "Failure triggered"

(** Given a defmap, a context, an expected type (which could be None if we are
    not enforcing a type), and an xexpr, return the evaluated version of the
    xexpr, its xtype, and a context associating xtypes to free variables. *)
and xexpr_eval (dm : defmap) (d : xcontext) (expected_type : xtype option)
    (xe : xexpr) : (expr * xtype * xcontext) optionE =
  let handle_ctrl_or_match (is_ctrl : bool) (xe0 : xexpr)
      (l : (xexpr * xexpr) list) (xelseopt : xexpr option) :
      ((expr * exprtype * exprtype * (expr * expr) list) * xtype * xcontext)
      optionE =
    begin
      let d_init =
        if (not is_ctrl) || expected_type = None || l = [] then
          SomeE d
        else begin
          (* This case happens when e.g. using ctrl as a pattern:
             we know the expected type but don't have the context
             for the free variables in xe0. *)
          match xexpr_eval dm StringMap.empty None (fst (List.hd l)) with
          | SomeE (_, _, fv0) -> begin
              match xexpr_eval dm fv0 expected_type (snd (List.hd l)) with
              | SomeE (_, _, fv) -> SomeE fv
              | NoneE err -> NoneE err
            end
          | NoneE err -> NoneE err
        end
      in
        match d_init with
        | NoneE err -> NoneE err
        | SomeE d_init -> begin
            match xexpr_eval dm d_init None xe0 with
            | NoneE err -> NoneE err
            | SomeE (e0, xt0, fv0) -> begin
                match map_merge true d_init fv0 with
                | NoneE err -> NoneE err
                | SomeE dfv0 -> begin
                    let l' =
                      all_or_nothing_optionE
                        begin
                          List.map
                            begin
                              fun (xej, xej') ->
                                match
                                  xexpr_eval dm StringMap.empty (Some xt0) xej
                                with
                                | SomeE (ej, _, dj) -> begin
                                    match map_merge true dfv0 dj with
                                    | SomeE dj' -> begin
                                        match
                                          xexpr_eval dm dj' expected_type xej'
                                        with
                                        | SomeE (ej', xt1, fvj) ->
                                            SomeE (ej, ej', xt1, fvj)
                                        | NoneE err -> NoneE err
                                      end
                                    | NoneE err -> NoneE err
                                  end
                                | NoneE err -> NoneE err
                            end
                            l
                        end
                    in
                      match l' with
                      | NoneE err -> NoneE err
                      | SomeE [] -> begin
                          match (xtype_eval dm xt0, expected_type) with
                          | SomeE t0, Some exp_type -> begin
                              match xtype_eval dm exp_type with
                              | SomeE t1 ->
                                  SomeE ((e0, t0, t1, []), exp_type, fv0)
                              | NoneE err -> NoneE err
                            end
                          | SomeE t0, None ->
                              SomeE ((e0, t0, Void, []), Void, fv0)
                          | NoneE err, _ -> NoneE err
                        end
                      | SomeE l' -> begin
                          let ejs = List.map fst4 l' in
                          let ej's = List.map snd4 l' in
                          let ejsej's = List.combine ejs ej's in
                          let xt1s = List.map trd4 l' in
                          let fvjs = List.map fth4 l' in
                          let xt1 = List.hd xt1s in
                          (* if not (List.for_all (( = ) xt1) xt1s) then
                               NoneE "Type mismatch"
                             else *)
                          let fv =
                            List.fold_left
                              begin
                                fun a b ->
                                  match a with
                                  | SomeE a -> map_merge true a b
                                  | NoneE err -> NoneE err
                              end
                              (SomeE StringMap.empty)
                              (map_exclusion dfv0 (map_dom d) :: fvjs)
                          in
                            match
                              (fv, xtype_eval dm xt0, xtype_eval dm xt1)
                            with
                            | SomeE fv, SomeE t0, SomeE t1 -> begin
                                match xelseopt with
                                | None -> SomeE ((e0, t0, t1, ejsej's), xt1, fv)
                                | Some xelse -> begin
                                    match
                                      xexpr_eval dm d_init expected_type xelse
                                    with
                                    | SomeE (eelse, _, _) -> begin
                                        match missing_span t0 ejs true with
                                        | None ->
                                            NoneE
                                              "Ortho check failed when \
                                               preprocessing else expression"
                                        | Some (mspan, _) ->
                                            SomeE
                                              ( ( e0,
                                                  t0,
                                                  t1,
                                                  ejsej's
                                                  @ List.map
                                                      (fun e -> (e, eelse))
                                                      mspan ),
                                                xt1,
                                                fv )
                                      end
                                    | NoneE err -> NoneE err
                                  end
                              end
                            | NoneE err, _, _
                            | _, NoneE err, _
                            | _, _, NoneE err ->
                                NoneE err
                        end
                  end
              end
          end
    end
  in
    if !debug_mode then
      Printf.printf
        "xexpr_eval:\ndm = %s\nd = %s\nxe = %s\nexpected_type = %s\n\n"
        (string_of_defmap dm) (string_of_xcontext d) (string_of_xexpr xe)
        (string_of_xtype_opt expected_type);
    let error_addition = Printf.sprintf "\nin %s" (string_of_xexpr xe) in
    let (out : (expr * xtype * xcontext) optionE) =
      begin
        match xe with
        | Null ->
            (* if expected_type <> None && expected_type <> Some Qunit then
                 NoneE (type_mismatch_message_expr expected_type Qunit)
               else *)
            SomeE (Null, Qunit, StringMap.empty)
        | Var x -> begin
            match StringMap.find_opt x d with
            | Some xt ->
                (* if expected_type <> None && expected_type <> Some xt then
                     NoneE (type_mismatch_message_expr expected_type xt)
                   else *)
                SomeE (Var x, xt, StringMap.empty)
            | None -> begin
                match expected_type with
                | None ->
                    NoneE
                      (Printf.sprintf "Unbound variable %s" x ^ error_addition)
                | Some exp_type ->
                    SomeE (Var x, exp_type, StringMap.singleton x exp_type)
              end
          end
        | Qpair (xe0, xe1) -> begin
            match
              begin
                match expected_type with
                | None -> (xexpr_eval dm d None xe0, xexpr_eval dm d None xe1)
                | Some expected_type -> begin
                    match xtype_reduce dm expected_type with
                    | SomeE (ProdType (t0, t1)) ->
                        ( xexpr_eval dm d (Some t0) xe0,
                          xexpr_eval dm d (Some t1) xe1 )
                    | SomeE _ ->
                        ( NoneE
                            (Printf.sprintf
                               "Type mismatch: expected expression of type \
                                %s, but got a pair"
                               (string_of_xtype expected_type)
                            ^ error_addition),
                          NoneE "" )
                    | NoneE err -> (NoneE err, NoneE err)
                  end
              end
            with
            | SomeE (e0, xt0, fv0), SomeE (e1, xt1, fv1) -> begin
                match map_merge true fv0 fv1 with
                | SomeE fv -> SomeE (Qpair (e0, e1), ProdType (xt0, xt1), fv)
                | NoneE _ ->
                    NoneE
                      ("Inconsistent inferred type of free variables"
                     ^ error_addition)
              end
            | NoneE err, _
            | _, NoneE err ->
                NoneE err
          end
        | Ctrl (xe0, l, xelseopt) -> begin
            match handle_ctrl_or_match true xe0 l xelseopt with
            | SomeE ((e0, t0, t1, l), xt1, fv) ->
                SomeE (Ctrl (e0, t0, t1, l), xt1, fv)
            | NoneE err -> NoneE (err ^ error_addition)
          end
        | Match (xe0, l, xelseopt) -> begin
            match handle_ctrl_or_match false xe0 l xelseopt with
            | SomeE ((e0, t0, t1, l), xt1, fv) ->
                SomeE (Match (e0, t0, t1, l), xt1, fv)
            | NoneE err -> NoneE (err ^ error_addition)
          end
        | Try (xe0, xe1) -> begin
            match
              ( xexpr_eval dm d expected_type xe0,
                xexpr_eval dm d expected_type xe1 )
            with
            | SomeE (e0, xt0, fv0), SomeE (e1, _, fv1) -> begin
                (* if xt0 <> xt1 then
                     NoneE
                       (Printf.sprintf
                          "Type mismatch in try-catch: try block has type %s, but \
                           catch block has type %s\n\
                           in"
                          (string_of_xtype xt0) (string_of_xtype xt1)
                       ^ error_addition)
                   else *)
                match map_merge true fv0 fv1 with
                | SomeE fv -> SomeE (Try (e0, e1), xt0, fv)
                | NoneE _ ->
                    NoneE
                      ("Inconsistent inferred type of free variables"
                     ^ error_addition)
              end
            | NoneE err, _
            | _, NoneE err ->
                NoneE (err ^ Printf.sprintf "\n%s" (string_of_xexpr xe))
          end
        | Apply (xf, xe) -> begin
            match xf with
            | ProgInvoke (name, params)
              when StringMap.find_opt name dm.constructors <> None ->
                (* Application of a constructor *)
                let typename, psig, _ = StringMap.find name dm.constructors in
                  begin
                    match
                      update_defmap_with_params dm StringMap.empty psig params
                    with
                    | SomeE dm' -> begin
                        match StringMap.find_opt typename dm.defs with
                        | Some (TypedefVariant (_, _, constructors)) -> begin
                            let names = List.map fst constructors in
                            let xtypes = List.map snd constructors in
                              if not (List.mem name names) then
                                NoneE
                                  (Printf.sprintf
                                     "Constructor %s not found\nin %s" name
                                     (string_of_xexpr xe))
                              else
                                let index = list_index ( = ) names name in
                                let initial_xtype = List.nth xtypes index in
                                  match
                                    ( all_or_nothing_optionE
                                        begin
                                          List.map
                                            begin
                                              fun xt ->
                                                match xtype_eval dm' xt with
                                                | SomeE xt' -> SomeE xt'
                                                | NoneE err ->
                                                    NoneE (err ^ error_addition)
                                            end
                                            xtypes
                                        end,
                                      (* Important that we use dm' for type reduction, but dm for evaluation! *)
                                      match xtype_reduce dm' initial_xtype with
                                      | SomeE initial_xtype' ->
                                          xexpr_eval dm d (Some initial_xtype')
                                            xe
                                      | NoneE err -> NoneE err )
                                  with
                                  | SomeE types, SomeE (e, _, fv) -> begin
                                      let types_path, types_tail =
                                        list_split_at_i types index
                                      in
                                      let initial_type = List.hd types_tail in
                                      let types_tail = List.tl types_tail in
                                      let tail_type =
                                        List.fold_right
                                          (fun (a : exprtype) (b : exprtype) ->
                                            if b = Void then
                                              a
                                            else
                                              SumType (a, b))
                                          types_tail Void
                                      in
                                      let initial_expr =
                                        if tail_type = Void then
                                          e
                                        else
                                          Apply
                                            (Left (initial_type, tail_type), e)
                                      in
                                      let e' =
                                        List.fold_right
                                          begin
                                            fun (t : exprtype)
                                                ((e', t') : expr * exprtype) ->
                                              ( Apply (Right (t, t'), e'),
                                                SumType (t, t') )
                                          end
                                          types_path
                                          ( initial_expr,
                                            if tail_type = Void then
                                              initial_type
                                            else
                                              SumType (initial_type, tail_type)
                                          )
                                        |> fst
                                      in
                                        SomeE
                                          ( e',
                                            TypeInvoke (typename, params),
                                            fv )
                                    end
                                  | NoneE err, _
                                  | _, NoneE err ->
                                      NoneE (err ^ error_addition)
                          end
                        | _ -> NoneE (Printf.sprintf "Name %s not found" name)
                      end
                    | NoneE err -> NoneE (err ^ error_addition)
                  end
            | _ -> begin
                match xprog_eval dm None expected_type xf with
                (* Case where type of expression is determined from program *)
                | SomeE (f, xt, xt') -> begin
                    match xexpr_eval dm d (Some xt) xe with
                    | SomeE (e, _, fv) -> SomeE (Apply (f, e), xt', fv)
                    | NoneE err -> NoneE err
                  end
                (* Case where type of program is determined from expression *)
                | NoneE _ -> begin
                    match xexpr_eval dm d None xe with
                    | SomeE (e, xt, fv) -> begin
                        match xprog_eval dm (Some xt) expected_type xf with
                        | SomeE (f, _, xt') -> SomeE (Apply (f, e), xt', fv)
                        | NoneE err -> NoneE err
                      end
                    | NoneE err -> NoneE err
                  end
              end
          end
        | ExprInvoke (name, params) -> begin
            match
              ( StringMap.find_opt name dm.expr_vars,
                StringMap.find_opt name dm.constructors,
                StringMap.find_opt name dm.defs )
            with
            | Some (e, xt, fv), _, _ -> SomeE (e, xt, fv)
            | _, Some _, _ -> begin
                xexpr_eval dm d expected_type
                  (Apply (ProgInvoke (name, params), Null))
              end
            | _, _, Some (Exprdef (_, psig, xt, body)) -> begin
                match update_defmap_with_params dm d psig params with
                | SomeE dm' -> begin
                    match xtype_reduce dm' xt with
                    | SomeE _ -> begin
                        (* if expected_type <> None && expected_type <> Some xt'
                           then
                             NoneE (type_mismatch_message_expr expected_type xt')
                           else *)
                        xexpr_eval dm' StringMap.empty expected_type body
                      end
                    | NoneE err -> NoneE err
                  end
                | NoneE err -> NoneE err
              end
            | _ -> NoneE (Printf.sprintf "Name %s not found" name)
          end
        | ExprIf (be, xe0, xe1) -> begin
            match bexpr_eval dm be with
            | SomeE true -> xexpr_eval dm d expected_type xe0
            | SomeE false -> xexpr_eval dm d expected_type xe1
            | NoneE err -> NoneE err
          end
        | ExprFail -> NoneE "Failure triggered"
      end
    in
      match out with
      | SomeE (e, xt, fv) -> begin
          match
            ( xtype_reduce dm xt,
              map_all_or_nothing_optionE (StringMap.map (xtype_reduce dm) fv)
            )
          with
          | SomeE xt', SomeE fv' -> begin
              if !debug_mode then
                Printf.printf
                  "Output of xexpr_eval:\n\
                   dm = %s\n\
                   d = %s\n\
                   expected_type = %s\n\
                   xe = %s\n\
                   e = %s\n\
                   xt = %s\n\
                   fv = %s\n\n"
                  (string_of_defmap dm) (string_of_xcontext d)
                  (string_of_xtype_opt expected_type)
                  (string_of_xexpr xe) (string_of_expr e) (string_of_xtype xt')
                  (string_of_xcontext fv');
              SomeE (e, xt', fv')
            end
          | NoneE err, _
          | _, NoneE err ->
              NoneE err
        end
      | NoneE err -> NoneE err

and xprog_eval (dm : defmap) (expected_in_type : xtype option)
    (expected_out_type : xtype option) (xf : xprog) :
    (prog * xtype * xtype) optionE =
  if !debug_mode then
    Printf.printf
      "xprog_eval:\n\
       dm = %s\n\
       xf = %s\n\
       expected_in_type = %s\n\
       expected_out_type = %s\n\n"
      (string_of_defmap dm) (string_of_xprog xf)
      (string_of_xtype_opt expected_in_type)
      (string_of_xtype_opt expected_out_type);
  match xf with
  | U3 (xr0, xr1, xr2) -> begin
      match (xreal_eval dm xr0, xreal_eval dm xr1, xreal_eval dm xr2) with
      | SomeE r0, SomeE r1, SomeE r2 ->
          (* begin
               match (in_type, expected_out_type) with
               | TypeInvoke ("Bit", []), None
               | TypeInvoke ("Bit", []), Some (TypeInvoke ("Bit", [])) ->
                   SomeE (U3 (r0, r1, r2), TypeInvoke ("Bit", []))
               | _ ->
                   NoneE
                     (type_mismatch_message_prog in_type expected_out_type
                        (TypeInvoke ("Bit", []))
                        (TypeInvoke ("Bit", [])))
             end *)
          SomeE
            (U3 (r0, r1, r2), TypeInvoke ("Bit", []), TypeInvoke ("Bit", []))
      | NoneE err, _, _
      | _, NoneE err, _
      | _, _, NoneE err ->
          NoneE err
    end
  | Lambda (xe0, xe1) -> begin
      match xexpr_eval dm StringMap.empty expected_in_type xe0 with
      | SomeE (e0, xt0, fv) -> begin
          match xexpr_eval dm fv expected_out_type xe1 with
          | SomeE (e1, xt1, _) -> begin
              (* if
                   xt0 <> in_type
                   || (expected_out_type <> None && Some xt1 <> expected_out_type)
                 then
                   NoneE
                     (type_mismatch_message_prog in_type expected_out_type xt0 xt1)
                 else *)
              match xtype_eval dm xt0 with
              | SomeE t0 -> SomeE (Lambda (e0, t0, e1), xt0, xt1)
              | NoneE err -> NoneE err
            end
          | NoneE err -> NoneE err
        end
      | NoneE err -> NoneE err
    end
  | Rphase (xe, xr0, xr1) -> begin
      match xexpr_eval dm StringMap.empty expected_in_type xe with
      | SomeE (e, xt, _) -> begin
          (* if
               xt <> in_type
               || (expected_out_type <> None && Some xt <> expected_out_type)
             then
               NoneE (type_mismatch_message_prog in_type expected_out_type xt xt)
             else *)
          match (xreal_eval dm xr0, xreal_eval dm xr1, xtype_eval dm xt) with
          | SomeE r0, SomeE r1, SomeE t -> SomeE (Rphase (t, e, r0, r1), xt, xt)
          | NoneE err, _, _
          | _, NoneE err, _
          | _, _, NoneE err ->
              NoneE err
        end
      | NoneE err -> NoneE err
    end
  | Pmatch l -> begin
      let l' =
        all_or_nothing_optionE
          begin
            List.map
              begin
                fun (xe0, xe1) ->
                  match xexpr_eval dm StringMap.empty expected_in_type xe0 with
                  | SomeE (e0, xt0, fv) -> begin
                      match xexpr_eval dm fv expected_out_type xe1 with
                      | SomeE (e1, xt1, _) -> begin
                          (* if
                               xt0 <> in_type
                               || expected_out_type <> None
                                  && Some xt1 <> expected_out_type
                             then
                               NoneE
                                 (type_mismatch_message_prog in_type
                                    expected_out_type xt0 xt1)
                             else *)
                          SomeE (e0, e1, xt0, xt1)
                        end
                      | NoneE err -> NoneE err
                    end
                  | NoneE err -> NoneE err
              end
              l
          end
      in
        match l' with
        | SomeE l' -> begin
            let e0s = List.map fst4 l' in
            let e1s = List.map snd4 l' in
            let xt0s = List.map trd4 l' in
            let xt1s = List.map fth4 l' in
            let xt0 = List.hd xt0s in
            let xt1 = List.hd xt1s in
              (* if
                   (not (List.for_all (( = ) xt0) xt0s))
                   || not (List.for_all (( = ) xt1) xt1s)
                 then
                   NoneE "Type mismatch"
                 else *)
              match (xtype_eval dm xt0, xtype_eval dm xt1) with
              | SomeE t0, SomeE t1 ->
                  SomeE (Pmatch (t0, t1, List.combine e0s e1s), xt0, xt1)
              | NoneE err, _
              | _, NoneE err ->
                  NoneE err
          end
        | NoneE err -> NoneE err
    end
  | ProgInvoke (name, params) -> begin
      match
        ( StringMap.find_opt name dm.prog_vars,
          StringMap.find_opt name dm.constructors,
          StringMap.find_opt name dm.defs )
      with
      | Some (f, xt0, xt1), _, _ -> begin
          (* if
               xt0 <> in_type
               || (expected_out_type <> None && Some xt1 <> expected_out_type)
             then
               NoneE
                 (type_mismatch_message_prog in_type expected_out_type xt0 xt1)
             else *)
          SomeE (f, xt0, xt1)
        end
      | _, Some _, _ -> begin
          xprog_eval dm expected_in_type expected_out_type
            (Lambda (Var "x", Apply (xf, Var "x")))
        end
      | _, _, Some (Progdef (_, psig, in_type', out_type, body)) -> begin
          match update_defmap_with_params dm StringMap.empty psig params with
          | SomeE dm' -> begin
              match (xtype_reduce dm' in_type', xtype_reduce dm' out_type) with
              | SomeE in_type', SomeE _ -> begin
                  (* if
                       in_type' <> in_type
                       || expected_out_type <> None
                          && Some out_type <> expected_out_type
                     then
                       NoneE
                         (type_mismatch_message_prog in_type expected_out_type
                            in_type' out_type)
                     else *)
                  match
                    xprog_eval dm' (Some in_type') expected_out_type body
                  with
                  | SomeE (f, in_type', out_type') ->
                      SomeE (f, in_type', out_type')
                  | NoneE err -> NoneE err
                end
              | NoneE err, _
              | _, NoneE err ->
                  NoneE err
            end
          | NoneE err -> NoneE err
        end
      | _ -> NoneE (Printf.sprintf "Name %s not found" name)
    end
  | ProgIf (be, xf0, xf1) -> begin
      match bexpr_eval dm be with
      | SomeE true -> xprog_eval dm expected_in_type expected_out_type xf0
      | SomeE false -> xprog_eval dm expected_in_type expected_out_type xf1
      | NoneE err -> NoneE err
    end
  | ProgFail -> NoneE "Failure triggered"

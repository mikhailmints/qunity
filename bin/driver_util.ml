open Qunity
open Util
open Matrix
open Syntax
open Extended_syntax
open Typechecking
open Semantics
open Gate
open Compilation

(*
Source:
https://baturin.org/blog/declarative-parse-error-reporting-with-menhir/
*)
module I = Parser.MenhirInterpreter

exception Syntax_error of ((int * int) option * string)

let get_lexing_position lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  let line_number = p.Lexing.pos_lnum in
  let column = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
    (line_number, column)

let get_parse_error env =
  match I.stack env with
  | (lazy Nil) -> "Invalid syntax"
  | (lazy (Cons (I.Element (state, _, _, _), _))) -> (
      try
        let message = Parser_messages.message (I.number state) in
          if message = "<YOUR SYNTAX ERROR MESSAGE HERE>\n" then
            "Invalid syntax"
          else
            message
      with
      | Not_found -> "Invalid syntax")

let rec parse lexbuf (checkpoint : 'a I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _env ->
      let token = Lexer.read lexbuf in
      let startp = lexbuf.lex_start_p and endp = lexbuf.lex_curr_p in
      let checkpoint = I.offer checkpoint (token, startp, endp) in
        parse lexbuf checkpoint
  | I.Shifting _
  | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
        parse lexbuf checkpoint
  | I.HandlingError _env ->
      let line, pos = get_lexing_position lexbuf in
      let err = get_parse_error _env in
        raise (Syntax_error (Some (line, pos), err))
  | I.Accepted v -> v
  | I.Rejected -> raise (Syntax_error (None, "Invalid syntax"))

let parse_string (s : string) : qunityfile =
  let lexbuf = Lexing.from_string s in
    parse lexbuf (Parser.Incremental.qunityfile lexbuf.lex_curr_p)

let parse_file (filename : string) : qunityfile =
  let lexbuf = Lexing.from_channel (open_in filename) in
    parse lexbuf (Parser.Incremental.qunityfile lexbuf.lex_curr_p)

let parse_with_err parse_fun s : qunityfile optionE =
  try SomeE (parse_fun s) with
  | Syntax_error (loc, err) -> begin
      let s =
        Printf.sprintf "Syntax error: %s\n%s" err
          begin
            match loc with
            | Some (line, pos) -> Printf.sprintf "At line %d, col %d" line pos
            | _ -> ""
          end
      in
        NoneE s
    end
  | Lexer.Lexing_error err -> NoneE err

let execute_expr (e : expr) : unit =
  match mixed_type_check StringMap.empty e with
  | NoneE err -> Printf.printf "Typechecking error: %s\n\n" err
  | SomeE tp -> begin
      Printf.printf "Expression type: %s\n\n%!"
        (string_of_type (type_of_mixed_expr_proof tp));
      Printf.printf "Pure semantics:\n%!";
      begin
        try print_mat (top_pure_expr_semantics e) with
        | Failure _
        | Invalid_argument _ ->
            Printf.printf "None\n"
      end;
      Printf.printf "\nMixed semantics:\n%!";
      print_mat (top_mixed_expr_semantics e);
      Printf.printf "\nPossible measurement outcomes:\n%!";
      let meas_outcomes = measurement_outcomes e in
      let _ =
        List.map
          (fun (e', p) ->
            Printf.printf "Probability %f: %s\n" p (string_of_expr e'))
          meas_outcomes
      in
      let error_prob =
        1. -. List.fold_left ( +. ) 0. (List.map snd meas_outcomes)
      in
        if not (float_approx_equal error_prob 0.) then
          Printf.printf "Probability %f: Error\n" error_prob;
        Printf.printf "\n"
    end

let get_expr_from_file (prog_filename : string) : expr optionE =
  let stdlib_filename = "bin/stdlib.qunity" in
    match parse_with_err parse_file stdlib_filename with
    | NoneE err -> NoneE (err ^ "\nin " ^ stdlib_filename)
    | SomeE stdlib_qf -> begin
        match parse_with_err parse_file prog_filename with
        | NoneE err -> NoneE (err ^ "\nin " ^ prog_filename)
        | SomeE { dm; main } -> begin
            let combined_dm = add_defmap stdlib_qf.dm dm in
              match main with
              | None -> NoneE "No main expression in file"
              | Some main -> begin
                  match xexpr_eval main combined_dm StringMap.empty with
                  | RNone err ->
                      NoneE (Printf.sprintf "Preprocessing error: %s" err)
                  | RExpr e -> SomeE e
                  | _ -> NoneE "Expected expression in main body"
                end
          end
      end

let compile_file (prog_filename : string) (out_filename : string)
    (gate_compiler : gate -> int -> int list -> int list -> string)
    (annotate : bool) : unit =
  let e_opt = get_expr_from_file prog_filename in
    match e_opt with
    | NoneE err ->
        Printf.printf "%s\n" err;
        exit 1
    | SomeE e -> begin
        match mixed_type_check StringMap.empty e with
        | NoneE err ->
            Printf.printf "Typechecking error: %s\n" err;
            exit 1
        | SomeE _ -> begin
            let gate, nqubits, out_reg, flag_reg = expr_compile annotate e in
            let qasm_str = gate_compiler gate nqubits out_reg flag_reg in
            let out_file = open_out out_filename in
              Printf.fprintf out_file "%s" qasm_str
          end
      end

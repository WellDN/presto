module T = struct
    include Tokens
end

open Ast

exception ParseError of string

type expected = Tok of T.t | Name of string

let pp_expected fmt = function
    | Tok tk -> T.pp fmt tk
    | Name s -> Format.pp_print_string fmt s 

let raise_error ~expected ~actual =
    let msg =
        Format.asprintf "Expected %a but found %a" pp_expected expected T.pp actual
    in
    raise (ParseError msg)

let expect expected tokens =
    let actual = Stream.next tokens in
    if actual <> expected then raise_error ~expected:(Tok expected) ~actual
    else ()

let expect_empty tokens =
    try Stream.empty tokens
    with Stream.Failure ->
        (* Stream.empty raises this error if stream isn't empty *)
        let bad_token = Stream.next tokens in
        raise_error ~expected:(Name "end of file") ~actual:bad_token

(* parsing grammar symbols *)

(* <identifier> ::= ? an identifier token ? *)
let parse_id tokens =
    match Stream.next tokens with
    | T.Identifier x -> x
    | other -> raise_error ~expected:(Name "an identifier") ~actual:other

(* <exp> ::= <int> *)
let parse_expression tokens =
    match Stream.next tokens with
    | T.Constant c -> Constant c (* perhaps should be -> Constant c. but i can pass (-> T.Constant c) / (-> c) have no idea of the difference but wcyd *)
    | other -> raise_error ~expected:(Name "an expression") ~actual:other

(* <statement> ::= "return" <exp> ";" *)
let parse_statement tokens =
    let _ = expect T.KWReturn tokens in
    let exp = parse_expression tokens in
    let _ = expect T.Semicolon tokens in
    Return exp 
    
(* <function> ::= "int" <identifier> "(" ")" "{" <statement> "}" *)
let parse_function_definition tokens =
  let _ = expect T.KWInt tokens in
  let fun_name = parse_id tokens in
  let _ =
    expect T.OpenParen tokens;
    (* Instead of expecting a KWVoid, allow an empty parameter list *)
    (match Stream.peek tokens with
    | Some T.CloseParen -> ()
    | _ -> raise_error ~expected:(Tok T.CloseParen) ~actual:(Stream.next tokens));
    expect T.CloseParen tokens;
    expect T.OpenBrace tokens
  in
  let statement = parse_statement tokens in
  let _ = expect T.CloseBrace tokens in
  Function { name = fun_name; body = statement }

let parse tokens =
    try
        let token_stream = Stream.of_list tokens in
        let fun_def = parse_function_definition token_stream in
        let _ = expect_empty token_stream in
        Program fun_def
    with Stream.Failure -> raise (ParseError "Unexpected end of file")

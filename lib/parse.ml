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

    (* Helper functions *)

let peek tokens = 
    match Stream.peek tokens with
    (* non-empty stream *)
    | Some t -> t
    | None -> raise Stream.Failure

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

(* return Some prec if token represents a binary operation, None otherwise *)
let get_precendence = function
    | T.Star | T.Slash | T.Percentage -> Some 50
    | T.Plus | T.Hyphen -> Some 45
    | T.(LessThan | LessOrEqual | GreaterThan | GreaterOrEqual) -> Some 35
    | T.(DoubleEqual | NotEqual) -> Some 30
    | T.LogicalAnd -> Some 10
    | T.LogicalOr -> Some 5
    | _ -> None

(* parsing grammar symbols *)

(* <identifier> ::= ? an identifier token ? *)
let parse_id tokens =
    match Stream.next tokens with
    | T.Identifier x -> x
    | other -> raise_error ~expected:(Name "an identifier") ~actual:other

(* <int> ::= ? A constant_token ? *)
let parse_constant tokens =
    match Stream.next tokens with
    | T.Constant c -> Constant c (* perhaps should be -> Constant c. but i can pass (-> T.Constant c) / (-> c) have no idea of the difference but wcyd *)
    | _ -> 
            raise(ParseError "Internal error when parsing constant") [@coverage off]

(* <unop> ::= "-" | "~" | "!" *)
let parse_unop tokens =
    match Stream.next tokens with
    | T.Tilde -> Complement
    | T.Hyphen -> Negate
    | T.Bang -> Not
    (* We only call this when we know next token is unop *)
    | _ ->
            raise (ParseError "Internal error parsing unary operator")
            [@coverage off]

    (* <binop> ::= "-" | "+" | "*" | "/" | "%" | "&&" | "||" | "==" | "!=" | "<" | ">" | "<=" | ">=" | *)
let parse_binop tokens =
    match Stream.next tokens with
    | T.Plus -> Add
    | T.Hyphen -> Subtract
    | T.Star -> Multiply
    | T.Slash -> Divide
    | T.Percentage -> Mod
    | T.LogicalAnd -> And
    | T.LogicalOr -> Or
    | T.DoubleEqual -> Equal
    | T.NotEqual -> NotEqual
    | T.LessThan -> LessThan
    | T.GreaterThan -> GreaterThan
    | T.LessOrEqual -> LessOrEqual
    | T.GreaterOrEqual -> GreaterOrEqual 
    | _ ->
            raise (ParseError "Internal error when parsing binary operator")
            [@coverage off]

(* <factor> ::= <int> | <unop> <factor> | "(" <exp> ")" *)
let rec parse_factor tokens =
    let next_token = peek tokens in
    match next_token with
    (*constant*)
    | T.Constant _ -> parse_constant tokens
    (* Unary *)
    | T.Hyphen | T.Tilde | T.Bang ->
            let operator = parse_unop tokens in
            let inner_exp = parse_factor tokens in
            Unary (operator, inner_exp)
    (* parenthesized expression *)   
    | T.OpenParen ->
            (* Stream.junk consumes open param *)
            let _ = Stream.junk tokens in
            let e = parse_expression 0 tokens in
            let _ = expect T.CloseParen tokens in
            e
    (* errors *)
    | t -> raise_error ~expected:(Name "a factor") ~actual:t

(* <exp> ::= <function> | <exp> <binop> <exp> *)
and parse_expression min_prec tokens =
    let initial_factor = parse_factor tokens in
    let next_token = peek tokens in
    let rec parse_exp_loop left next =
        match get_precendence next with
        | Some prec when prec >= min_prec ->
                let operator = parse_binop tokens in
                let right = parse_expression (prec + 1) tokens in 
                let left = Binary (operator, left, right) in
                parse_exp_loop left (peek tokens)
                | _ -> left
    in
    parse_exp_loop initial_factor next_token

(* <statement> ::= "return" <exp> ";" *)
let parse_statement tokens =
    let _ = expect T.KWReturn tokens in
    let exp = parse_expression 0 tokens in
    let _ = expect T.Semicolon tokens in
    Return exp 
    
(* <function> ::= "int" <identifier> "(" ")" "{" <statement> "}" *)
let parse_function_definition tokens = 
    let _ = expect KWInt tokens in
    let function_name = parse_id tokens in
    let _ =
        expect T.OpenParen tokens;
        expect T.KWVoid tokens; 
        expect T.CloseParen tokens; 
        expect T.OpenBrace tokens 
    in
    let statement = parse_statement tokens in
    let _ = expect T.CloseBrace tokens in
    Function { name = function_name; body = statement }

let parse tokens =
    try
        let token_stream = Stream.of_list tokens in
        let fun_def = parse_function_definition token_stream in
        let _ = expect_empty token_stream in
        Program fun_def
    with Stream.Failure -> raise (ParseError "Unexpected end of file")

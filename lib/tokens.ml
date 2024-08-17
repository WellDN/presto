[@@@coverage exclude_file]
type t =
  (* tokens with contents *)
  | Identifier of string
  | Constant of int
  (* Keywords *)
  | KWInt
  | KWReturn
  | KWVoid
  | KWIf
  | KWElse
  (* punctuation *)
  | OpenParen
  | CloseParen
  | OpenBrace
  | CloseBrace
  | Semicolon
  | DoubleHyphen
  | Hyphen
  | Tilde
  | Plus
  | Star
  | Slash
  | Percentage
  | Bang
  | LogicalAnd
  | LogicalOr
  | DoubleEqual
  | NotEqual
  | LessThan
  | GreaterThan
  | LessOrEqual
  | GreaterOrEqual
  | EqualSign 
  | Colon
  | QuestionMark
[@@deriving show]

module T = struct
    include Tokens
end


exception ParseError of string

type expected = Tok of T.t | Name of string

let pp_expected fmt = function
    | Tok tk -> T.pp fmt tk
    | Name s -> Format.pp_print_string fmt s 

 

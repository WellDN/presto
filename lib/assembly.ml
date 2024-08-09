type reg = AX | R10 | R11 | DX
type operand = Imm of int | Reg of reg | Pseudo of string | Stack of int
type unary_operator = Neg | Not
type binary_operator = Add | Sub | Mult

type instruction = 
    | Mov of operand * operand
    | Unary of unary_operator * operand
    | Binary of { op: binary_operator; src: operand; dst: operand }
    | Idiv of operand
    | Cdq (* sign-extension to EDX (for division) (cause idivl dst (division) treats EDX and EAX as single and calculates [EDX:EAX]/ dst *)
    | AllocateStack of int
    | Ret

type function_definition = 
    | Function of { name : string; instructions : instruction list }

type t = Program of function_definition

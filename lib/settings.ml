type stage = Lex | Parse | Codegen | Assembly | Executable | Tacky | Validate
type target = OS_X | Linux

let platform = ref OS_X (* default to OS X *)
let debug = ref false

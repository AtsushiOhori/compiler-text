structure Instruction = 
struct
  datatype prim = EQ | ADD | SUB | MUL | DIV
  datatype inst 
    = PushI of int | PushS of string | PushB of bool
    | Acc of string | App | Pair | Proj1 | Proj2
    | Prim of prim | MkCLS of string * inst list
    | MkREC of string * string * inst list
    | If of inst list * inst list | Ret
  type C = inst list
(*
  fun instToString inst = 
      case inst of
        PushI int => "PushI(" ^ Int.toString int ^ ")"
      | PushS string => "PushS(\"" ^ string ^ "\")"
      | PushB bool => "PushB(" ^ Bool.toString bool ^ ")"
      | Acc string => "Acc(" ^ string ^ ")"
      | App => "App"
      | Pair => "Pair"
      | Proj1 => "Proj1"
      | Proj2 => "Proj2"
      | Prim EQ => "Prim(EQ)"
      | Prim ADD => "Prim(ADD)"
      | Prim SUB => "Prim(SUB)"
      | Prim MUL => "Prim(MUL)"
      | Prim DIV => "Prim(DIV)"
      | MkCLS (x, code) => "MkCls(" ^ x ^ "," ^ codeToString code ^ ")"
      | MkREC (f, x, code)
        => "MkCls(" ^ f ^ "," ^ x ^ "," ^ codeToString code ^ ")"
      | If (code1, code2) => 
        "If(" ^ codeToString code1 ^ "," ^ codeToString code2 ^ ")"
      | Ret => "Ret"
  and codeToString code = 
      "[" ^ String.concatWith "," (map instToString code) ^ "]"
*)
  fun instToString inst = Dynamic.format inst
  fun codeToString C = Dynamic.format C
end

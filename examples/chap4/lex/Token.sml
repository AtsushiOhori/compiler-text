structure Token = struct
  datatype token
    = EOF | UNDERBAR | ID of string
    | STRING of string | REAL of string
    | SPECIAL of string
fun toString token =
    case token of
      EOF => "EOF "
    | ID s => "ID " ^ s
    | REAL s => "REAL " ^ s
    | STRING s => "STRING " ^ "\"" ^ s ^ "\""
    | UNDERBAR => "UNDERBAR "
    | SPECIAL s => "SPECIAL" ^ s
end

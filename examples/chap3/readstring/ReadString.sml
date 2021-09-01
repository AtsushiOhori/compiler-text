structure ReadString =
struct
  exception EOF
  fun skipSpaces inStream = 
    if TextIO.endOfStream inStream then raise EOF
    else case TextIO.lookahead inStream of
           SOME c => 
           if Char.isSpace c then 
             (TextIO.input1 inStream; skipSpaces inStream)
           else ()
         | NONE => ()
  fun readString inStream =
    let
      fun readRest s =
         case TextIO.lookahead inStream of
           SOME c => if Char.isSpace c then s
                     else (TextIO.input1 inStream; 
                           readRest (s ^ str c))
         | NONE => s
    in 
      readRest ""      
    end
end

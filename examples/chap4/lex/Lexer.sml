structure Lexer =
struct
 exception EOF
 fun makeLexer inStream =
   let val lexer = 
         CoreMLLex.makeLexer
         (fn n => case TextIO.input1 inStream of
                  SOME c => str c | NONE => "")
   in fn () => let val token = lexer ()
               in if token = Token.EOF then raise EOF
                  else token
               end
   end
end

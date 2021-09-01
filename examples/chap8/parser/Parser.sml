structure Parser =
struct
  exception EOF
  exception ParseError = CoreMLLrVals.Parser.ParseError
  structure P = CoreMLLrVals.Parser
  structure T = CoreMLLrVals.Tokens
  type stream = P.stream
  fun print_error (s,pos1,pos2) = 
    print ("Syntax error(" 
           ^ Int.toString pos1 
           ^ "-" ^ Int.toString pos2 ^ ") :" ^ s ^ "\n")
  fun discardSemicolons stream = 
    let val (token, rest) = P.getStream stream
    in if P.sameToken (token, T.SEMICOLON (0,0)) then 
        discardSemicolons rest
       else if P.sameToken (token, T.EOF (0,0)) then raise EOF 
       else stream
    end
  fun doParse stream =
    let val stream = discardSemicolons stream
        val (dec, stream) =
          P.parse {lookahead=0, stream=stream, 
                   error=print_error,arg=()}
        val _ = print ("Parse result:\n" 
                       ^ (Syntax.decToString dec) ^ "\n")
    in (dec, stream) end
  fun makeStream inStream = 
   let val lexer = CoreMLLex.makeLexer
                   (fn n => TextIO.inputN (inStream,1))
   in P.makeStream {lexer=lexer} end
end

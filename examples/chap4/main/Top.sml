structure Top =
struct
  fun readAndPrintLoop lexer =
    let 
      val token = lexer()
      val _ = print (Token.toString token ^ "\n")
    in
      readAndPrintLoop lexer
    end
  fun top file =
    let 
      val inStream = TextIO.openIn file
      val lexer = Lexer.makeLexer inStream
    in 
       readAndPrintLoop lexer;
       TextIO.closeIn inStream
    end
    handle Lexer.EOF => ()
end

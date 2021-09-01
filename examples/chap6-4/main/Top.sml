structure Top =
struct
  open Parser
  exception NotImplemented
  fun readAndPrintLoop stream =
    let
      val (dec, stream) = doParse stream
      val _ = Typeinf.typeinf dec
    in
      readAndPrintLoop stream
    end
  fun top file =
    let
      val inStream = case file of 
                       "" => TextIO.stdIn
                     | _ => TextIO.openIn file
      val stream = Parser.makeStream inStream
    in
      readAndPrintLoop stream 
      handle Parser.EOF => ()
           | Parser.ParseError => 
             (print "Syntax error\n"; ())
           | Typeinf.TypeError => 
             (print "Type error\n"; ());
      case file of "" => () 
                 | _ => TextIO.closeIn inStream
    end
end

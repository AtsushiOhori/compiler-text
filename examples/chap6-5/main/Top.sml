structure Top =
struct
  open Parser
  exception NotImplemented
  fun readAndPrintLoop gamma stream =
    let
      val (dec, stream) = doParse stream
      val newGamma = Typeinf.typeinf gamma dec
    in
      readAndPrintLoop newGamma stream
    end
  fun top file =
    let
      val inStream = case file of 
                       "" => TextIO.stdIn
                     | _ => TextIO.openIn file
      val stream = Parser.makeStream inStream
      val gamma = TypeUtils.emptyTyEnv
    in
      readAndPrintLoop gamma stream 
      handle Parser.EOF => ()
           | Parser.ParseError => 
             (print "Syntax error\n"; ())
           | Typeinf.TypeError => 
             (print "Type error\n"; ());
      case file of "" => () 
                 | _ => TextIO.closeIn inStream
    end
end

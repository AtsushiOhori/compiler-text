structure Top =
struct
  fun readAndPrintLoop env gamma stream =
    let
      val (dec, stream) = Parser.doParse stream
      val newGamma = Typeinf.typeinf gamma dec
      val namedCode = Comp.compile dec
      val newEnv = Exec.run env namedCode
    in
      readAndPrintLoop newEnv newGamma stream
    end
  fun top file =
    let
      val inStream = case file of 
                       "" => TextIO.stdIn
                     | _ => TextIO.openIn file
      val stream = Parser.makeStream inStream
      val gamma = TypeUtils.emptyTyEnv
      val env = Value.emptyEnv
    in
      readAndPrintLoop env gamma stream 
      handle Parser.EOF => ()
           | Parser.ParseError => print "Syntax error\n"
           | Typeinf.TypeError => print "Type error\n"
           | Exec.RuntimeError => print "Runtime error\n";
      case file of "" => () 
                 | _ => TextIO.closeIn inStream
    end
end

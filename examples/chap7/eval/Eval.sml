structure Eval = struct
  structure S = Syntax
  structure V = Value
  exception RuntimeError
  fun evalExp env exp = 
    case exp of
      S.EXPID string => 
      (case SEnv.find(env, string) of
         SOME v => v
       | _ => raise RuntimeError)
    | S.INT int => V.INT int
    | S.STRING string => V.STRING string
    | S.TRUE => V.BOOL true
    | S.FALSE => V.BOOL false
    | S.EXPFN (string, exp) => V.CLS(env, string, exp)
    | S.EXPAPP  (exp1, exp2) =>
      let val v1 = evalExp env exp1
          val v2 = evalExp env exp2
      in case v1 of
           V.CLS(env1, x, exp1) =>
           evalExp (SEnv.insert(env1, x, v2))  exp1
         | V.REC(env1, f, x, exp1) => 
           evalExp (SEnv.insert
                   (SEnv.insert(env1, f,v1), x, v2))
                   exp1
         | _ => raise RuntimeError
      end
    | S.EXPPROJ1 exp =>
      let
        val v = evalExp env exp
      in 
        case v of
          V.PAIR (v1,v2) => v1
        | _ => raise RuntimeError
      end
    | S.EXPPROJ2 exp =>
      let
        val v = evalExp env exp
      in 
        case v of
          V.PAIR (v1,v2) => v2
        | _ => raise RuntimeError
      end
    | S.EXPPRIM (prim, exp1, exp2) =>
      let
        val v1 = evalExp env exp1
        val v2 = evalExp env exp2
        val arg = 
            case (v1,v2) of
              (V.INT i1, V.INT i2) => (i1, i2)
            | _ => raise RuntimeError
      in 
        case prim of
          S.ADD => V.INT (op + arg)
        | S.SUB => V.INT (op - arg)
        | S.MUL => V.INT (op * arg)
        | S.DIV => V.INT (op div arg)
        | S.EQ => V.BOOL (op = arg)
      end
    | S.EXPPAIR (exp1, exp2) =>
      let
        val v1 = evalExp env exp1
        val v2 = evalExp env exp2
      in 
        V.PAIR(v1,v2)
      end
    | S.EXPIF (exp1, exp2, exp3) =>
      let
        val v1 = evalExp env exp1
      in 
        case v1 of
          V.BOOL true => evalExp env exp2
        | V.BOOL false => evalExp env exp3
        | _ => raise RuntimeError
      end
    | S.EXPFIX (string1, string2, exp) =>
      V.REC(env, string1, string2,  exp)
  fun eval env (S.VAL (id, exp)) =
    let val v = evalExp env exp
    in print ("Evaluated to:\n" 
              ^ "val " ^ id ^ " = "
              ^ Value.valueToString v ^ "\n");
       SEnv.insert(env, id, v)
    end
end


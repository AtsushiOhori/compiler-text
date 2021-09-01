(**
 * Type inference module
 * @author Atsushi Ohori
 *)
structure Typeinf = struct
  open Type Syntax TypeUtils UnifyTy
  exception TypeError
  exception TypeError
  fun PTS absyn =
    case absyn of
      INT int => (emptyTyEnv, INTty)
    | STRING string => (emptyTyEnv, INTty)
    | TRUE => (emptyTyEnv, BOOLty)
    | FALSE => (emptyTyEnv, BOOLty)
    | EXPID string => 
        let
          val newty = newTy()
        in
          (singletonTyEnv(string, newty), newty)
        end
    | EXPPAIR (exp1, exp2) =>
        let
          val (tyEnv1, ty1) = PTS exp1
          val (tyEnv2, ty2) = PTS exp2
          val tyEquations = matches (tyEnv1, tyEnv2)
          val subst = unify tyEquations
          val tEnv3 = 
              unionTyEnv
                (substTyEnv subst tyEnv1,
                 substTyEnv subst tyEnv2)
        in
          (tEnv3, substTy subst (PAIRty(ty1, ty2)))
        end
    | EXPAPP (exp1, exp2) =>
        let
          val (tyEnv1, ty1) = PTS exp1
          val (tyEnv2, ty2) = PTS exp2
          val tyEquations = matches (tyEnv1, tyEnv2)
          val newty = newTy()
          val subst = unify ((FUNty(ty2, newty), ty1)
                            :: tyEquations)
          val tyEnv3 = 
              unionTyEnv
                (substTyEnv subst tyEnv1,
                 substTyEnv subst tyEnv2)
        in
          (tyEnv3, substTy subst newty)
        end
    | EXPFN (string, exp) =>  
        let
          val (tyEnv, ty) = PTS exp
        in
          case findTyEnv(tyEnv, string) of
            SOME domty =>
            (removeTyEnv(tyEnv, string),
             FUNty(domty, ty))
          | NONE => (tyEnv, FUNty(newTy(), ty))
        end
    | _ => raise TypeError
  fun typeinf  dec =
      let
        val exp = case dec of 
                    Syntax.VAL (id, exp) => exp
        val (tyEnv, ty) = PTS exp
        val _ = print 
                  ("Inferred Typing:\n"
                   ^ TypeUtils.tyEnvToString tyEnv
                   ^ " |- "
                   ^ Syntax.expToString exp
                   ^  " : "
                   ^ Type.tyToString ty
                   ^ "\n"
                  )
      in
        ()
      end
end

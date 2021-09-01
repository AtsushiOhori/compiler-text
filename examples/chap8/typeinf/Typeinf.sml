(**
 * Type inference module
 * @author Atsushi Ohori
 *)
structure Typeinf = struct
  open Syntax Type TypeUtils UnifyTy
  exception TypeError
  infixr ++
  fun s1 ++ s2 = composeSubst s1 s2
fun W gamma exp = 
  case exp of
    INT (int) => (emptySubst, INTty)
  | EXPID (string) => 
    (case SEnv.find(gamma, string) of
       SOME ty => (emptySubst, freshInst ty)
     | NONE => raise TypeError)
  | EXPFN (string, exp) =>  
    let val ty1 = newTy()
        val newGamma = SEnv.insert(gamma, string, ty1)
        val (S, ty2) = W newGamma exp
    in 
      (S, FUNty(substTy S ty1, ty2))
    end
  | EXPAPP (exp1, exp2) =>
    let
      val (S1, ty1) = W gamma exp1
      val (S2, ty2) = W (substTyEnv S1 gamma) exp2
      val ty3 = newTy()
      val S3 = unify [(FUNty(ty2, ty3), substTy S2 ty1)]
      val S4 = composeSubst S3 (composeSubst S2 S1)
    in
      (S4, substTy S4 ty3)
    end
  | STRING (string) => (emptySubst, STRINGty)
  | TRUE => (emptySubst, BOOLty)
  | FALSE => (emptySubst, BOOLty)
  | EXPPAIR (exp1, exp2) =>
    let
      val (S1, ty1) = W gamma exp1
      val (S2, ty2) = W (substTyEnv S1 gamma) exp2
    in
      (
       S2 ++ S1,
       PAIRty(substTy S2 ty1,ty2)
      )
    end
  | EXPPROJ1 exp =>
    let
      val (S1, ty) = W gamma exp
      val ty1 = newTy()
      val ty2 = newTy()
      val S2 = unify [(ty, PAIRty (ty1, ty2))]
    in
      (
       S2 ++ S1,
       substTy S2 ty1
      )
    end
  | EXPPROJ2 exp =>
    let
      val (S1, ty) = W gamma exp
      val ty1 = newTy()
      val ty2 = newTy()
      val S2 = unify [(ty, PAIRty (ty1, ty2))]
    in
      (
       S2 ++ S1,
       substTy S2 ty2
      )
    end
    | EXPIF (exp1, exp2, exp3) => 
        let
          val (S1, ty1) = W gamma exp1
          val S2 = unify [(ty1, BOOLty)]
          val (S3, ty2) = W (substTyEnv (S2 ++ S1) gamma) exp2
          val (S4, ty3) = W (substTyEnv (S3 ++ S2 ++ S1) gamma) exp3
          val S5 = unify [(ty2, ty3)]
          val S = S5 ++ S4 ++ S3 ++ S2 ++ S1
          val newGamma = substTyEnv S gamma
        in
          (S, substTy S5 ty2)
        end
    | EXPFIX (fid, xid, exp) =>  
        let
          val argTy = newTy()
          val bodyTy = newTy()
          val funTy = FUNty(argTy, bodyTy)
          val newGamma = 
              SEnv.insert(SEnv.insert(gamma, fid, funTy),
                          xid, argTy)
          val (S1, ty) = W newGamma exp
          val S2 = unify [(ty, bodyTy)]
          val S = S2 ++ S1
        in
          (S, substTy S funTy)
        end
    | EXPPRIM (p, exp1, exp2) =>
      let
        val (S1, ty1) = W gamma exp1
        val (S2, ty2) = W (substTyEnv S1 gamma) exp2
        val S3 = unify [(substTy S2 ty1, INTty), (ty2, INTty)]
        val ty3 = 
            case p of EQ => BOOLty | _ => INTty
      in
        (S3 ++ S2 ++ S1, ty3)
      end

  fun typeinf gamma (VAL (id, exp)) =
    let
      val (subst, ty) = W gamma exp
      val tids = SSet.listItems (FTV ty)
      val newTy = if null tids then ty else POLYty (tids, ty)
      val _ = print ("Inferred typing:\n"
                     ^ "val " ^ id ^ " : "
                     ^ Type.tyToString newTy ^ "\n")
    in
      SEnv.insert(gamma, id,  newTy)
    end
    handle Unify => raise TypeError
end

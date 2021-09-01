(**
 * Type inference module
 * @author Atsushi Ohori
 *)
structure W = struct
local
  open Absyn Types TypeUtils Unify
in
  exception NotImplemented
  exception TypeError
  infixr ++
  fun s1 ++ s2 = composeSubst s1 s2
  fun W gamma absyn = 
    case absyn of
      INT (int) => (emptySubst, INTty)
    | STRING (string) => (emptySubst, STRINGty)
    | TRUE => (emptySubst, BOOLty)
    | FALSE => (emptySubst, BOOLty)
    | EXPID (string) => 
        (case SEnv.find(gamma, string) of
           SOME ty => (emptySubst, freshInst ty)
         | NONE => raise TypeError
        )
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
    | EXPAPP (exp1, exp2) =>
        let
          val (S1, ty1) = W gamma exp1
          val (S2, ty2) = W (substTyEnv S1 gamma) exp2
          val newty = newTy()
          val S3 = unify [(FUNty(ty2, newty), substTy S2 ty1)]
        in
          (
           S3 ++ S2 ++ S1,
           substTy S3 newty
          )
        end
    | EXPFN (string, exp) =>  
        let
          val newty = newTy()
          val newGamma = SEnv.insert(gamma, string, newty)
          val (S, ty) = W newGamma exp
        in
          (S,
           FUNty(substTy S newty, ty)
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
    | EXPPRIM (prim, exp1, exp2) => 
      case prim of
        "eq" =>
        let
          val (S1, ty1) = W gamma exp1
          val (S2, ty2) = W (substTyEnv S1 gamma) exp2
          val S3 = unify [(ty1, ty2)]
        in
          (S3 ++ S2 ++ S1, BOOLty)
        end
      | _ =>
        let
          val (S1, ty1) = W gamma exp1
          val (S2, ty2) = W (substTyEnv S1 gamma) exp2
          val S3 = unify [(substTy S2 ty1, INTty), (ty2, INTty)]
        in
          (S3 ++ S2 ++ S1, INTty)
        end

  fun typeinf gamma (Absyn.VAL (id, exp)) =
      let
        val (subst, ty) = W gamma exp
        val tids = SSet.listItems (Unify.FTV ty)
        val newTy = if null tids then ty else Types.POLYty (tids, ty)
        val _ = 
            print (
            "Inferred typing:\n"
            ^ "val " 
            ^ id
            ^ " : "
            ^ Types.tyToString newTy
            ^ "\n")
        val newGamma = SEnv.insert(gamma, id,  newTy)
      in
        newGamma
      end
    | typeinf gamma _ = raise TypeError
end
end

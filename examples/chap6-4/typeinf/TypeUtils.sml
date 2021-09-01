structure TypeUtils =
struct
local
  open Type
in
  type subst = ty SEnv.map
  val emptySubst = SEnv.empty
  fun substTy subst ty = 
      case ty of 
      INTty => ty
    | STRINGty => ty
    | BOOLty => ty
    | TYVARty string => 
      (case SEnv.find (subst, string) of
        NONE => ty
      | SOME ty => ty)
    | FUNty (ty1, ty2) => 
      FUNty (substTy subst ty1, substTy subst ty2)
    | PAIRty (ty1, ty2) =>
      PAIRty (substTy subst ty1, substTy subst ty2)
    | POLYty (tids, ty) =>
      POLYty (tids, substTy subst ty)
  fun composeSubst subst1 subst2 = 
    SEnv.unionWith
      (fn (ty1, ty2) => ty1)
      (SEnv.map (substTy subst1) subst2,
       subst1)
  type tyEnv = ty SEnv.map
  val findTyEnv = SEnv.find
  fun substTyEnv subst tyEnv =
      SEnv.map (substTy subst) tyEnv
  val emptyTyEnv = SEnv.empty
  fun singletonTyEnv (tyID, ty) = SEnv.singleton (tyID, ty)
  fun matches (tyEnv1, tyEnv2) =
    SEnv.listItems
      (SEnv.intersectWith (fn x => x) (tyEnv1, tyEnv2))
  fun unionTyEnv (tyEnv1, tyEnv2) =
      SEnv.unionWith #1 (tyEnv1, tyEnv2)
  fun removeTyEnv (tyEnv, string) = #1 (SEnv.remove(tyEnv, string))
fun freshInst ty = 
  case ty of
    POLYty (tids, ty) =>
    let val S = 
          foldr (fn (tid, S) =>
                    let val newty = newTy ()
                    in SEnv.insert(S, tid, newty) end)
                emptySubst
                tids
        in substTy S ty end
    | _ => ty

  fun tyEnvToString tyEnv = 
      let
        val stringTyList = SEnv.listItemsi tyEnv
      in
        "{" ^ (String.concatWith " , " 
               (map (fn (id,ty) => id ^ ":" ^ tyToString ty) stringTyList))
            ^ "}"
      end
end
end

structure Type =
struct
  local
    val nextTyId = ref 0
    fun newTyId () = (!nextTyId before nextTyId := !nextTyId + 1)
  in
    fun initSeed () = nextTyId := 0
    fun newTyIdName () =
      let
        fun tyIdName tid = 
            let
              fun numeral n = 
                  if n < 26
                  then [ord #"a" + n]
                  else 
                    let val (msb, rest) = (n mod 26, (n div 26) - 1)
                    in (ord #"a" + msb) :: (numeral  rest)
                    end
            in
              (implode(map chr (rev (numeral tid))))
            end
      in
        tyIdName (newTyId())
      end
  end
  datatype ty =
      INTty
    | STRINGty
    | BOOLty
    | TYVARty of string
    | FUNty of ty * ty
    | PAIRty of ty * ty
    | POLYty of string list * ty
  fun newTy () = TYVARty (newTyIdName())
  fun tyToString ty = 
      case ty of
      INTty => "int"
    | STRINGty => "string"
    | BOOLty => "bool"
    | TYVARty string  => "'" ^ string
    | FUNty (ty1, ty2) => 
      "(" ^ tyToString ty1  ^ " -> " ^ tyToString ty2 ^ ")"
    | PAIRty (ty1, ty2) => 
      "(" ^ tyToString ty1  ^ " * " ^ tyToString ty2 ^ ")"
    | POLYty (tids, ty) =>
      "[" 
      ^
      String.concatWith "," tids
      ^
      "."
      ^
      tyToString ty
      ^
      "]"
(*
  fun tyToString ty = Dynamic.format ty
*)
end

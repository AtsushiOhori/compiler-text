(**
 * expression syntax
 * @copyright (c) 2006, Tohoku University.
 * @author Atsushi Ohori
 *)
structure Syntax = 
struct
  datatype prim = EQ | ADD | SUB | MUL | DIV
  datatype exp
    = EXPID of  string | INT of int | STRING of string 
    | TRUE | FALSE | EXPFN of string * exp 
    | EXPAPP of exp * exp | EXPPAIR of exp * exp 
    | EXPPROJ1 of exp | EXPPROJ2 of exp 
    | EXPPRIM of prim *  exp * exp
    | EXPIF of exp * exp * exp 
    | EXPFIX of string * string * exp
  and dec 
    = VAL of string * exp
  fun expToString exp =
      case exp of
        INT int => Int.toString int
      | STRING string => "\"" ^ string ^ "\""
      | TRUE => "true"
      | FALSE => "false"
      | EXPID string => string
      | EXPPAIR (exp1, exp2) => 
        "(" ^ expToString exp1 ^ "," ^ expToString exp2 ^ ")"
      | EXPAPP (exp1, exp2) =>
        "(" ^ expToString exp1 ^ " " ^ expToString exp2 ^ ")"
      | EXPIF (exp1, exp2, exp3) =>
        "if " 
         ^ expToString exp1
         ^ " then "
         ^ expToString exp2
         ^ " else "
         ^ expToString exp3
      | EXPFN (string, exp) =>
        "(fn " ^ string ^ " => " ^ expToString exp ^ ")"
      | EXPPROJ1 exp => "#1 " ^ expToString exp
      | EXPPROJ2 exp => "#2 " ^ expToString exp
      | EXPFIX (f, x, exp) =>
        "(fix " 
        ^ f 
        ^ "("
        ^ x 
        ^ ") => " ^ expToString exp ^ ")"
      | EXPPRIM (p, exp1, exp2) =>
        let
          val prim = case p of ADD => "add" | SUB => "sub"
                             | MUL => "mul" | DIV => "div"
                             | EQ => "eq"
        in
          "prim(" ^ prim ^ "," ^ expToString exp1 ^ "," ^ expToString exp2 ^ ")"
        end
  and decToString dec =
      case dec of
        VAL (x, exp) =>
        "val " ^ x ^ " = " ^ expToString exp
(*
  fun printExp exp = print (expToString exp)
  fun printDec dec = print (decToString dec)
  fun expToString exp = Dynamic.format exp
  fun decToString dec = Dynamic.format dec
*)
end

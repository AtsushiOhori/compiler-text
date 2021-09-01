structure Eval =
struct
  open TM
  fun Hd nil = B
    | Hd (h :: _) = h
  fun Tl nil = nil 
    | Tl (_ :: tl) = tl
  fun Cons (B, nil) = nil
    | Cons (h,t) = h::t
  fun moveL (LList, h, RList) = 
      (Tl LList, Hd LList, Cons (h, RList))
  fun moveR (LList, h, RList) = 
      (Cons (h, LList), Hd RList, Tl RList)
  fun move L tape = moveL tape
    | move R tape = moveR tape
  fun print (q, (LList, h, RList)) =
      Dynamic.pp
      {state=q,
       tape = (List.rev LList, h, RList)}
  fun exec delta (q, tape as (LList, h, RList)) =
       case List.find (fn (x,y) => x = (q, h)) delta of
         NONE => (LList, h, RList)
       | SOME (x, (q', s, d)) => 
         exec delta (q', move d (LList, s, RList))
  fun eval (state, delta) tape = exec delta (state,tape)
end

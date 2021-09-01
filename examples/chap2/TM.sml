structure TM =
struct
  datatype D = R | L
  datatype S = B | I | O 
  datatype Q = M | H
  type delta = ((Q * S) * (Q * S * D)) list
  type program = Q * delta
  type tape = S list *  S * S list
  val P = (M, [((M, I), (M, O, L)),
               ((M, O), (H, I, L)),
               ((M, B), (H, I, L))
          ])
end

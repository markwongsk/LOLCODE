structure PrintUtil =
struct
  
  fun listToString f l =
    "[" ^ (List.foldl (fn (x,y) => y ^ ", " ^ (f x)) "" l) ^ "]"
end

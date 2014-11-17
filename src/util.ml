let repeat n v =
  let rec aux acc n =
    if n = 0 then acc
    else aux (v :: acc) (pred n)
  in aux [] n

let rec print_list p = function
  | [] -> ()
  | [x] -> p x
  | x :: xs -> p x; print_string "; "; print_list p xs

let rec ( -- ) init final =
  if init > final then []
  else init :: (succ init -- final)

let ( --^ ) init final =
  init -- pred final

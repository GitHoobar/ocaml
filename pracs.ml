let rec fact int = 
  match int with
  | 0 -> 1
  | x -> x * fact(x-1);;

print_int (fact 6);;

let rec remove_duplicates list = 
  match list with
  | [] -> []
  | [x] -> [x]
  | first :: second :: tl ->
    if first = second then
      remove_duplicates(second :: tl)
    else 
      first :: remove_duplicates(second :: tl);;
    
let string_of_list lst =
  "[" ^ String.concat "; " (List.map string_of_int lst) ^ "]";;

print_endline (string_of_list (remove_duplicates [1;1;2;2;3;3;4;5;6]));;


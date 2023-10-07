let rec read_inputs acc = 
  print_string "Enter a number: ";
  flush stdout;
  let input = read_line () in
  try
    match input with
    | "quit" -> acc
    | "" -> read_inputs (0 :: acc)
    | _ -> read_inputs (int_of_string input :: acc)
  with
    Failure _ -> print_endline "Invalid input. Please enter a valid number"; 
    read_inputs acc
let rec find_max acc max = function 
  | [] -> max
  | x :: xs -> match x with
              | 0 -> if acc > max then find_max 0 acc xs else find_max 0 max xs
              | _ -> find_max (x + acc) max xs
let main () = 
  let inouts = read_inputs [] in
  print_endline "Result: ";
  List.iter (fun x -> print_endline (string_of_int x)) inouts;
  print_endline ("Max: " ^ string_of_int (find_max 0 0 inouts))
let () = main ()

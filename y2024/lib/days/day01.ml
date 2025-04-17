let read_lines filename =
  let ch = open_in filename in
  let rec loop acc1 acc2 =
    try
      let line = input_line ch in
      let parts = line |> String.split_on_char ' ' |> List.filter (fun s -> s <> "") in
      match parts with
      | part1 :: part2 :: _ ->
        let num1 = int_of_string part1 in
        let num2 = int_of_string part2 in
        loop (num1 :: acc1) (num2 :: acc2)
      | _ -> loop acc1 acc2
    with
    | End_of_file ->
      close_in ch;
      (* Reverse and convert to arrays *)
      Array.of_list (List.sort compare acc1), Array.of_list (List.sort compare acc2)
  in
  loop [] []
;;

let array1, array2 = read_lines "inputs/task1.txt"

let sum =
  Array.mapi (fun i a -> Int.abs (a - array2.(i))) array1 |> Array.fold_left ( + ) 0
;;

let str_sum = string_of_int sum
let print_part1 = print_endline str_sum

let count_occurrences value array =
  Array.fold_left (fun acc x -> if x = value then acc + 1 else acc) 0 array
;;

let sum =
  Array.map (fun a -> a * count_occurrences a array2) array1 |> Array.fold_left ( + ) 0
;;

let str_sum = string_of_int sum
let print_part2 = print_endline str_sum

let rec delta lst =
  match lst with
  | x1 :: x2 :: x3 -> (x2 - x1) :: delta (x2 :: x3)
  | _ -> []

let read_lines filename =
  let ch = open_in filename in
  let rec loop acc =
    try
      let line = input_line ch in
    let parsed = line
        |> String.split_on_char ' '
        |> List.filter (fun s -> s <> "")
        |> List.map (fun s -> int_of_string s)
        |> delta in
      loop (parsed :: acc)
    with End_of_file ->
      close_in ch;
      List.rev acc
  in
  loop []

let lines = read_lines "inputs/task2.txt"

let safe_row row =
  List.for_all (fun x -> x > 0 && x <= 3) row ||
    List.for_all (fun x -> x < 0 && x >= -3) row

let rec sum_safe_lists list =
  match list with
    | [] -> 0
    | h :: t -> match safe_row h with
        | true -> 1 + sum_safe_lists t
        | false -> sum_safe_lists t

let result = sum_safe_lists lines

let print_part1 = print_endline (string_of_int result)

let rec row_direction row =
    match row with
    | [] -> 0
    | h :: t -> h + row_direction t

let safe_jump jump direction =
    match direction with
    | negative when negative < 0 -> jump < 0 && jump >= -3
    | positive when positive > 0 -> jump > 0 && jump <= 3
    | _ -> false

let damped_lines list =
    List.map (fun x ->
        let direction = row_direction x in
        let rec damped_row acc row =
            match row with
            | [] -> acc
            | h :: t -> match safe_jump h direction with
                | true -> damped_row (h :: acc) t
                | false -> List.append acc t
        in
        damped_row [] x
    ) list


let result = sum_safe_lists (damped_lines lines)

let print_part2 = print_endline (string_of_int result)

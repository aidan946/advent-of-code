open Core

let read_file_to_string filename = In_channel.read_all filename
let re = Re.Perl.re "mul\\(([0-9]+),([0-9]+)\\)" |> Re.compile

let all_matches str =
  Re.all re str
  |> List.map ~f:(fun g ->
    let x = Re.Group.get g 1 |> Int.of_string in
    let y = Re.Group.get g 2 |> Int.of_string in
    x * y)
;;

let part1 filename =
  let content = read_file_to_string filename in
  let results = all_matches content in
  List.fold results ~init:0 ~f:( + )
;;

let print_part1 =
  let result = part1 "inputs/task3.txt" in
  Printf.printf "Day 03 Part 1: %d\n" result
;;

let part2 filename =
  let content = read_file_to_string filename in
  let re2 = Re.Perl.re "mul\\(([0-9]+),([0-9]+)\\)|do\\(\\)|don't\\(\\)" |> Re.compile in
  let matches = Re.all re2 content in
  let _, sum =
    List.fold matches ~init:(true, 0) ~f:(fun (enabled, acc) g ->
      let matched = Re.Group.get g 0 in
      if String.equal matched "do()"
      then true, acc
      else if String.equal matched "don't()"
      then false, acc
      else if enabled
      then (
        let x = Re.Group.get g 1 |> Int.of_string in
        let y = Re.Group.get g 2 |> Int.of_string in
        enabled, acc + (x * y))
      else enabled, acc)
  in
  sum
;;

let print_part2 =
  let result = part2 "inputs/task3.txt" in
  Printf.printf "Day 03 Part 2: %d\n" result
;;

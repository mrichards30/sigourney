#use "dictionary.ml";;
open Dictionary;;

let distance_between (origin: string) (target: string) : int = 
  [0; 1; 2; 3] 
  |> List.map (fun n -> if (String.get origin n) = (String.get target n) then 0 else 1) 
  |> List.fold_left (+) 0;;
  
let find_adjacent_nodes (origin: string) : string list = 
  List.filter (fun word -> distance_between origin word = 1) Dictionary.four_letter_words;;

let rec shortest_list (current: ('a list) option) (lst: 'a list list) : ('a list) option = 
  match (lst, current) with
    | ((x :: xs), Some c) -> shortest_list (Some (if List.length x < List.length c then x else c)) xs
    | ((x :: xs), None) -> shortest_list (Some x) xs
    | _ -> current;;

let rec find_path_between (origin: string) (target: string): string list option = 
  let rec aux (origin: string) (target: string) (path: string list) = 
    if origin = target then Some (target :: path)
    else if List.length path > 4 then None
    else origin 
      |> find_adjacent_nodes 
      |> List.filter (fun node -> List.for_all (fun e -> e <> node) path)
      |> List.map (fun node -> aux node target (origin :: path))
      |> List.filter (fun node -> Option.is_some node)
      |> List.map (fun node -> Option.get node)
      |> shortest_list None
  in aux origin target [];;

#use "dictionary.ml";;
open Dictionary;;

let distance_between (origin: string) (target: string) : int = 
  [0; 1; 2; 3] 
  |> List.map (fun n -> if (String.get origin n) = (String.get target n) then 0 else 1) 
  |> List.fold_left (+) 0;;
  
let find_adjacent_nodes (origin: string) : string list = 
  List.filter (fun word -> distance_between origin word = 1) Dictionary.four_letter_words;;

let rec shortest_list (lst: 'a list list) : ('a list) option = 
  List.fold_left (fun a b -> 
    match a with
      | Some x -> Some (if List.length x < List.length b then x else b)
      | None -> Some b) 
  None lst;;

let rec find_path_between (origin: string) (target: string): string list option = 
  let max_depth = 3 in
  let rec aux (origin: string) (target: string) (path: string list) = 
    if origin = target then Some (target :: path)
    else if List.length path > max_depth - 1 then None
    else origin 
      |> find_adjacent_nodes 
      |> List.filter (fun node -> List.for_all (fun e -> e <> node) path)
      |> List.map (fun node -> aux node target (origin :: path))
      |> List.filter (fun node -> Option.is_some node)
      |> List.map (fun node -> Option.get node)
      |> shortest_list
  in aux origin target [];;

#use "dictionary.ml";;
open Dictionary;;
open Queue;;
open Hashtbl;;

let distance_between (origin: string) (target: string) : int = 
  [0; 1; 2; 3] 
  |> List.map (fun n -> if (String.get origin n) = (String.get target n) then 0 else 1) 
  |> List.fold_left (+) 0;;

let find_adjacent_nodes (origin: string) : string list = 
  List.filter (fun word -> distance_between origin word = 1) Dictionary.four_letter_words;;

let rec backtrack (map: (string, string) Hashtbl.t) (initial: string) (target: string): string list =
  let parent = Hashtbl.find map target in
  if parent = initial then [initial]
  else parent :: (backtrack map initial parent);;

let bfs (initial: string) (target: string): string list = 
  let queue = Queue.queue_of [initial] in
  let visited: (string list) ref = ref [initial] in
  let parent_map: (string, string) Hashtbl.t = Hashtbl.create 300 in
  while not (Queue.is_empty queue) do 
    match Queue.dequeue queue with
    | Some t -> 
      if t = target then 
        queue := []
      else
        let adjs = find_adjacent_nodes t in
        let filtered_adjs = List.filter (fun e -> not (List.mem e !visited)) adjs in
        Queue.enqueue_all filtered_adjs queue;
        visited := List.flatten [!visited; filtered_adjs];
        List.fold_left (
          fun a b -> 
            Hashtbl.add parent_map b t
        ) () filtered_adjs;
    | None -> ()
  done;
  List.rev (target :: backtrack parent_map initial target);;

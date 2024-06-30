module WordGraph = struct
  let create_dictionary (length: int) : string list =
    match length with 
    | 4 -> Dictionary.four_letter_words
    | 5 -> Dictionary.five_letter_words
    | _ -> raise @@ Invalid_argument "No associated dictionary found" 
    
  let distance_between (origin: string) (target: string) : int = 
    let rec aux a b idx acc =
      if idx = String.length a then acc else 
      let delta = if String.get a idx = String.get b idx then 0 else 1
      in (aux [@tailcall]) a b (1 + idx) (delta + acc)
    in aux origin target 0 0
  
  let find_adjacent_nodes (origin: string) (dictionary: string list) : string list = 
    List.filter (fun word -> distance_between origin word = 1) dictionary

  let find_neighbours word = 
    let dictionary = create_dictionary @@ String.length word in
    find_adjacent_nodes word dictionary
end
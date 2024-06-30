module type Graph = sig
  val find_neighbours : string -> string list 
end

module PathFinder (G : Graph) = struct 
  open Queue

  let rec backtrack map initial target =
    let parent = Hashtbl.find map target in
    if parent = initial then [initial]
    else parent :: (backtrack map initial parent)

  let run_bfs (initial: 'a) (target: 'a): 'a list =
    let queue = Queue.queue_of [initial] in
    let visited: ('a list) ref = ref [initial] in
    let parent_map: ('a, 'a) Hashtbl.t = Hashtbl.create 300 in
    while not (Queue.is_empty queue) do 
      match Queue.dequeue queue with
        | Some t -> 
          if t = target then 
            Queue.empty queue 
          else
            let filter_visited = List.filter (fun e -> not (List.mem e !visited)) in 
            let unvisited_neighbours = filter_visited @@ G.find_neighbours t in
            Queue.enqueue_all queue unvisited_neighbours;
            visited := List.flatten [!visited; unvisited_neighbours];
            List.fold_left (
              fun _ b -> 
                Hashtbl.add parent_map b t
            ) () unvisited_neighbours;
        | None -> ()
    done;
    target :: backtrack parent_map initial target
end

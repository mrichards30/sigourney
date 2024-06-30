module Queue = struct
  type queue = (string list) ref

  let empty: queue = ref []

  let enqueue (x: string) (q: queue): unit = 
    q := !q @ [x]

  let rec enqueue_all (lst: string list) (q: queue): unit = 
    match lst with 
    | [] -> ()
    | x :: xs -> 
      enqueue x q; 
      enqueue_all xs q

  let dequeue (q: queue): string option =
    match !q with 
    | [] -> None
    | x :: xs -> 
      q := xs; 
      Some x

  let is_empty (q: queue): bool = 
    List.length !q = 0

  let queue_of (l: string list): queue =
    let new_queue = empty in 
    enqueue_all l new_queue;
    new_queue
end
module Queue = struct
  type 'a queue = ('a list) ref;;

  let empty: 'a queue = ref [];;

  let enqueue (x: 'a) (q: 'a queue): unit = 
    q := !q @ [x];;

  let queue_of (lst: 'a list): 'a queue = 
    let new_queue: 'a queue = empty in
    let rec aux l = 
      match l with 
      | x :: xs -> enqueue x new_queue; aux xs
      | [] -> new_queue in 
    aux lst;;

  let rec enqueue_all (lst: 'a list) (q: 'a queue): unit = 
    match lst with 
    | [] -> ()
    | x :: xs -> 
      enqueue x q; 
      enqueue_all xs q;;

  let dequeue (q: 'a queue): 'a option =
    match !q with 
    | [] -> None
    | x :: xs -> 
      q := xs; 
      Some x;;

  let is_empty (q: 'a queue): bool = 
    List.length !q = 0;;
end
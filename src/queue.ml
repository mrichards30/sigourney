module Queue = struct
  type 'a queue = ('a list * 'a list) ref

  let bq (front: 'a list) (rear: 'a list) = 
    match front with
      | [] -> (List.rev (rear), [])
      | _ -> (front, rear)

  let enqueue (q: 'a queue) (x: 'a): unit = 
    let (front, rear) = !q in 
    q := bq front (x::rear)

  let enqueue_all (q: 'a queue) (lst: 'a list): unit = 
    List.iter (enqueue q) lst

  let dequeue (q: 'a queue): 'a option =
    let (front, rear) = !q in
    match front with 
      | [] -> None
      | x::xs -> 
        q := bq xs rear; 
        Some x

  let is_empty (q: 'a queue): bool = 
    let (front, rear) = !q in
    List.length front = 0 && List.length rear = 0

  let empty (q: 'a queue): unit = 
    q := ([], [])

  let queue_of (lst: 'a list): 'a queue = 
    let q: 'a queue = ref ([], []) in
    enqueue_all q lst;
    q
end
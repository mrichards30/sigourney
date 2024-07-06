open Traversal

module WordPathFinder = PathFinder (Wordgraph.WordGraph)
  
let list_to_dotpoints lst = 
  lst |> List.map (fun word -> "<li>" ^ word ^ "</li>") 
      |> List.fold_left (^) ""

let () = 
  let open Js_of_ocaml in
  let initial_input = Js.Unsafe.global##.document##getElementById "initialText" in
  let target_input = Js.Unsafe.global##.document##getElementById "targetText" in
  let search_button = Js.Unsafe.global##.document##getElementById "search" in
  let answer_list = Js.Unsafe.global##.document##getElementById "answerList" in
  let run_bfs _ =
    let initial = initial_input##.value |> Js.to_string |> String.lowercase_ascii in
    let target = target_input##.value |> Js.to_string |> String.lowercase_ascii in
    if String.length initial = String.length target then
      let res = WordPathFinder.run_bfs target initial in
      answer_list##.innerHTML := Js.string @@ list_to_dotpoints res; Js._true
    else 
      Js_of_ocaml.Js.Unsafe.global##alert (Js_of_ocaml.Js.string "Enter words of the same length into each box")
  in
  search_button##.onclick := Dom_html.handler run_bfs;
  search_button##.ontouchstart := Dom_html.handler run_bfs
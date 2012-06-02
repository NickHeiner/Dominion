module Utils

type proceed = Stop | Continue

let rec foldlSome (items : 'el list) (f : 'acc -> 'el -> ('acc * proceed)) (init : 'acc) =
  match items with
  | [] -> init
  | hd::tl -> match f init hd with
               | acc, Stop -> acc
               | acc, Continue -> foldlSome tl f acc

let withNth items index item =
  let rec helper currIndex items = 
    if currIndex < 0 then failwith "invalid index" else
      match items with
        | hd::tl -> if currIndex <> 0 then hd::(helper (currIndex - 1) tl) else item::tl
        | [] -> failwith "invalid index"
  helper index items

(* Returns a list with the same elements as the original but in randomized order *)
let shuffle items = items
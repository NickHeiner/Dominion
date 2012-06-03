module Utils

type proceed = Stop | Continue

let withNth items index item =
  let rec helper currIndex items = 
    if currIndex < 0 then invalidArg "index" (sprintf "invalid index: %d" currIndex) else
      match items with
        | hd::tl -> if currIndex <> 0 then hd::(helper (currIndex - 1) tl) else item::tl
        | [] -> invalidArg "index" (sprintf "invalid index: %d" currIndex)
  helper index items

(* Returns a list with the same elements as the original but in randomized order *)
let shuffle items = items

let listMem list item = List.exists ((=) item) list

let withIndices items = Seq.zip (seq { 0 .. List.length items}) (items |> List.toSeq) |> Seq.toList

let prettyPrintCardCounts cardCounts = cardCounts
                                          |> Map.toList
                                          |> List.map (fun (card, count) -> sprintf "\t%A\t%d" card count)
                                          |> String.concat "\n"
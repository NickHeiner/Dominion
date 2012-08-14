module Utils

type proceed = Stop | Continue

let withNth items index item =
  let rec helper currIndex items = 
    if currIndex < 0 then invalidArg "index" (sprintf "invalid index: %d" currIndex) else
      match items with
        | hd::tl -> if currIndex <> 0 then hd::(helper (currIndex - 1) tl) else item::tl
        | [] -> invalidArg "index" (sprintf "invalid index: %d" currIndex)
  helper index items

let countOccurences list item = List.filter (fun x -> x = item) list |> List.length

(* Returns a list with the same elements as the original but in randomized order *)
let shuffle items = items

let listMem list item = List.exists ((=) item) list

let withIndices items = Seq.zip (seq { 0 .. List.length items}) (items |> List.toSeq) |> Seq.toList

let prettyPrintCardCounts cardCounts = cardCounts
                                          |> Map.toList
                                          |> List.map (fun (card, count) -> sprintf "\t%A\t%d" card count)
                                          |> String.concat "\n"

let allCards (player : Definitions.player) = player.hand @ player.discard @ player.deck

(* Returns a list without the first item for which pred returns true *)
let rec withoutFirst pred = function
    | hd::tl when pred hd -> tl
    | hd::tl -> hd::(withoutFirst pred tl)
    | [] -> []

let contains item list =
    match List.tryFind ((=) item) list with
        | Some _ -> true
        | None -> false

let withoutNone items = List.fold (fun list -> function Some el -> el::list | None -> list) [] items |> List.rev

let memCount items = items |> Seq.countBy id |> Map.ofSeq

let ensureSubset super sub = 
    List.fold (fun (subAcc, superAcc) subEl -> if contains subEl superAcc
                                                then (subAcc, withoutFirst ((=) subEl) superAcc)
                                                else (withoutFirst ((=) subEl) subAcc, superAcc)) (sub, super) sub
    |> fst

let fillHand src dest =
    let disjointSrc = List.fold (fun acc el -> withoutFirst ((=) el) acc) src dest
    let rec helper src dest =
        if List.length dest >= Constants.MILITIA_DRAW_DOWN_COUNT
        then dest 
        else
            match src with
            | [] -> dest
            | hd::tl -> helper tl (hd::dest)
    helper disjointSrc dest

let pick set = match Set.toList set with
                | [] -> failwith "Expected more than 0 elements"
                | hd::[] -> hd
                | _::_::_ -> failwith <| sprintf "Expected set %A to contain only a single element" set
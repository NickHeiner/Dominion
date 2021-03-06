﻿module Utils

open Definitions

type proceed = Stop | Continue

let withNth items index item =
  let rec helper currIndex items = 
    if currIndex < 0 then invalidArg "index" (sprintf "invalid index: %d" currIndex) else
      match items with
        | hd::tl -> if currIndex <> 0 then hd::(helper (currIndex - 1) tl) else item::tl
        | [] -> invalidArg "index" (sprintf "invalid index: %d" currIndex)
  helper index items

let countOccurences list item = List.filter ((=) item) list |> List.length
let countOccurs item list = countOccurences list item

(* It's necessary to construct a single rand instance and reuse it.
   http://stackoverflow.com/questions/11975161/f-system-random-next-returning-the-same-result *)
let rand = System.Random()

(* Returns a list with the same elements as the original but in randomized order *)
let shuffle items = 
    items
    |> List.map (fun x -> (x, rand.Next()))
    |> List.sortBy snd
    |> List.map fst

let listMem list item = List.exists ((=) item) list

let prettyPrintCardCounts cardCounts = cardCounts
                                          |> Map.toList
                                          |> List.map (fun (card, count) -> sprintf "\t%A\t%f" card count)
                                          |> String.concat "\n"

let allCards (player : Definitions.player) = player.hand @ player.discard @ player.deck

(* Returns a list without the first item for which pred returns true *)
let rec withoutFirst pred = function
    | hd::tl when pred hd -> tl
    | hd::tl -> hd::(withoutFirst pred tl)
    | [] -> []

let drop item = withoutFirst ((=) item)

let equalWithoutLog game game' = {game with log = []} = {game' with log = []}

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

let nth index items = List.nth items index

let defaultFind key ifNotFound map =
    match Map.tryFind key map with
    |   Some value -> value
    |   None -> ifNotFound 

let fst3 (x, _, _) = x
let mid  (_, x, _) = x
let thd  (_, _, x) = x

let toString x = sprintf "%A" x

let pick set = match Set.toList set with
                | [] -> invalidArg "set" "Expected more than 0 elements"
                | hd::[] -> hd
                | _::_::_ -> invalidArg "set" <| sprintf "Expected set %A to contain only a single element" set

(* from http://blogs.msdn.com/b/dsyme/archive/2009/11/08/equality-and-comparison-constraints-in-f-1-9-7.aspx *)
let compareOn f x (yobj: obj) =
    match yobj with
    | :? 'T as y -> compare (f x) (f y)
    | _ -> invalidArg "yobj" "cannot compare values of different types"

let equalsOn f x (yobj:obj) =
        match yobj with
        | :? 'T as y -> (f x = f y)
        | _ -> false
 
let hashOn f x =  hash (f x)

let toObj items = Seq.map (fun x -> x :> obj) items
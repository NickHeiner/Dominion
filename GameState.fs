module GameState

open Definitions
open Constants

(* Turn for a player. purchasingPower doesn't include coins. *)
type turn = {actions : int; buys : int; purchasingPower : int}
(* This would be defined in Constants if not for mutual recursion issues *)
let initialTurn = {actions = 1; buys = 1; purchasingPower = 0}  

(* Complete game state *)
type t = {players : player list; cards : Map<card, int>; trash : card list; currentTurn : turn}

(* Universally publicly visible state *)
type sanitized = {discard : card list list; cards : Map<card, int>; trash: card list}

let initialGameState = {players = []; cards = List.fold (fun acc el -> Map.add el (initialCount el) acc) Map.empty allCards; 
                        trash = []; currentTurn = initialTurn }

let sanitize gameState = {discard = List.map (fun (player : player) -> player.discard) gameState.players;
                            cards = gameState.cards; trash = gameState.trash}
                            
let updatePlayer id (update : player -> player) gameState = List.nth gameState.players id |> update |> Utils.withNth gameState.players id

let withTurn turn gameState = {gameState with currentTurn = turn}
let addActions count gameState = withTurn {gameState.currentTurn with actions = gameState.currentTurn.actions + count} gameState
let addBuys count gameState = withTurn {gameState.currentTurn with actions = gameState.currentTurn.buys + count} gameState
let addPurchasingPower amount gameState = withTurn {gameState.currentTurn with actions = gameState.currentTurn.purchasingPower + amount} gameState

let rec draw player count =
  if count = 0 then player else 
    match player.deck with 
      | hd::tl -> draw {player with hand = hd::player.hand; deck = tl} (count - 1)
      | [] -> draw {player with deck = Utils.shuffle player.discard} count


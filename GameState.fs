module GameState

open Definitions
open Constants

(* This would be defined in Constants if not for mutual recursion issues *)
let initialTurn = {actions = 1; buys = 1; purchasingPower = 0}  

(* Universally publicly visible state *)
type sanitized = {discard : card list list; cards : Map<card, int>; trash: card list}

let initialGameState = {players = List.replicate 2 initialPlayer; cards = List.fold (fun acc el -> Map.add el (initialCount el) acc) Map.empty allCards; 
                        trash = []; currentTurn = initialTurn; turnsTaken = 0 }

let nextTurn gameState = {gameState with currentTurn = initialTurn; turnsTaken = gameState.turnsTaken + 1}

let sanitize gameState = {discard = List.map (fun (player : player) -> player.discard) gameState.players;
                            cards = gameState.cards; trash = gameState.trash}
                            
let updatePlayer id (update : player -> player) gameState =
  {gameState with players = List.nth gameState.players id |> update |> Utils.withNth gameState.players id}

let withTurn turn gameState = {gameState with currentTurn = turn}
let addActions count gameState = withTurn {gameState.currentTurn with actions = gameState.currentTurn.actions + count} gameState
let addBuys count gameState = withTurn {gameState.currentTurn with actions = gameState.currentTurn.buys + count} gameState
let addPurchasingPower amount gameState = withTurn {gameState.currentTurn with actions = gameState.currentTurn.purchasingPower + amount} gameState

let totalPurchasingPower id gameState = 
  gameState.currentTurn.purchasingPower + List.sumBy purchasingPowerOf (List.nth gameState.players id).hand

let rec draw count player =
  if count = 0 then player else 
    match player.deck with 
      | hd::tl -> draw (count - 1) {player with hand = hd::player.hand; deck = tl}
      | [] -> draw count {player with deck = Utils.shuffle player.discard; discard = []} 

let discardAll player = {player with discard = player.hand @ player.discard; hand = [] }

let buy id card gameState =
  let availableMoney = totalPurchasingPower id gameState
  let cost = cardCost card 
  if availableMoney < cost then failwith (sprintf "%d is not enough to buy card %A" availableMoney cost) else
    (* TODO update player purchasing power *)
    updatePlayer id (fun player -> {player with discard = card::player.discard}) gameState

module GameState

open Definitions
open Constants

(* This would be defined in Constants if not for mutual recursion issues *)
let initialTurn = {actions = 1; buys = 1; purchasingPower = 0}  

(* Universally publicly visible state *)
type sanitized = {discard : card list list; cards : Map<card, int>; trash: card list}

let initialGameState = {players = []; cards = List.fold (fun acc el -> Map.add el (initialCount el) acc) Map.empty allCards; 
                        trash = []; currentTurn = initialTurn; turnsTaken = 0 }

let nextTurn gameState = {gameState with currentTurn = initialTurn; turnsTaken = gameState.turnsTaken + 1}

let sanitize gameState = {discard = List.map (fun (player : player) -> player.discard) gameState.players;
                            cards = gameState.cards; trash = gameState.trash}

let getPlayer id gameState = List.nth gameState.players id
                            
let updatePlayer id (update : player -> player) gameState =
  {gameState with players = getPlayer id gameState |> update |> Utils.withNth gameState.players id}

let withTurn turn gameState = {gameState with currentTurn = turn}
let addActions count gameState = withTurn {gameState.currentTurn with actions = gameState.currentTurn.actions + count} gameState
let addBuys count gameState = withTurn {gameState.currentTurn with buys = gameState.currentTurn.buys + count} gameState
let addPurchasingPower amount gameState = withTurn {gameState.currentTurn with purchasingPower = gameState.currentTurn.purchasingPower + amount} gameState

let totalPurchasingPower id gameState = 
  gameState.currentTurn.purchasingPower + List.sumBy purchasingPowerOf (List.nth gameState.players id).hand

let rec draw count player =
  if count = 0 then player else 
    match player.deck with 
      | hd::tl -> draw (count - 1) {player with hand = hd::player.hand; deck = tl}
      | [] -> draw count {player with deck = Utils.shuffle player.discard; discard = []} 

let drawFor count id = updatePlayer id (fun player -> draw count player)

let discardAll player = {player with discard = player.hand @ player.discard; hand = [] }

type requireCardInHand = Yes | No

let removeCard getDiscard requireCard card id gameState =
    match List.tryFind ((=) card) (getPlayer id gameState).hand with
        | Some _ -> updatePlayer id
                        (fun player -> {player with hand = Utils.withoutFirst ((=) card) player.hand; discard = getDiscard card player.discard})
                        gameState
        | None -> match requireCard with
                    | Yes -> invalidArg "card" (sprintf "Player %d does not have card %A in hand" id card)
                    | No -> gameState

let safeDiscard card = removeCard (fun card discard -> card::discard) No card
let discard card = removeCard (fun card discard -> card::discard) Yes card
let trash = removeCard (fun _ discard -> discard) Yes
    
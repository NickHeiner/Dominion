module ActionCards

open Definitions
open Constants

type action = int -> gameState -> gameState

let actionOfCard = function
  | Smithy -> fun id gameState -> GameState.updatePlayer id (GameState.draw SMITHY_CARDS_DRAW) gameState
  | Cellar toDiscard -> fun id gameState -> List.fold (fun gameState card -> gameState
                                                                                |> GameState.discard card id
                                                                                |> GameState.drawFor 1 id)
                                                gameState toDiscard
                                           |> GameState.addActions 1
  | Chapel (card1, card2, card3, card4) -> fun id gameState -> List.fold
                                                                (fun gameState card -> GameState.trash card id gameState) gameState
                                                                (Utils.withoutNone [card1; card2; card3; card4])
  | Chancellor reshuffle -> fun id gameState -> 
                                (match reshuffle with
                                    | Reshuffle -> GameState.updatePlayer id
                                                    (fun player -> {player with deck = []; discard = player.discard @ player.deck}) gameState
                                    | NoReshuffle -> gameState) |> GameState.addPurchasingPower CHANCELLOR_PURCHASING_POWER
  | Village -> fun id gameState -> gameState
                                        |> GameState.addActions 1
                                        |> GameState.drawFor 1 id
  | Woodcutter -> fun id gameState -> gameState
                                        |> GameState.addPurchasingPower WOODCUTTER_PURCHASING_POWER
                                        |> GameState.addBuys WOODCUTTER_BUYS 
  | Feast toGain as feast -> if cardCost toGain > 5
                                then invalidArg "toGain"
                                         (sprintf "Feast can only give a card costing up to 5, but %A costs %d" toGain (cardCost toGain))
                                else fun id gameState -> gameState
                                                            |> GameState.trash (Action feast) id
                                                            |> GameState.updatePlayer id (fun player ->
                                                                {player with discard = toGain::player.discard})
  (* | Militia -> fun id gameState -> { 0 .. List.length gameState.players} 
                                    |> Seq.filter (fun x -> x <> id)
                                    |> Seq.fold (fun game id -> GameState.updatePlayer id
                                                                    (fun player -> let card1, card2, card3 = player.militiaReaction player.hand
                                                                                   let hand = Utils.withoutNone [card1; card2; card3]
                                                                                   {player with hand = hand}) game)
                                        gameState *)
  | _ -> failwith "not impl"
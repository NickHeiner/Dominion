module ActionCards

open Definitions
open Constants

type action = int -> gameState -> gameState

(* Maybe validation should be separated out from the actual action logic.
   One advantage of not separating it out is that it's easier to be more generous with fall-back behavior, 
   instead of just flatly rejecting ill-formed actions. *)
(* there are many edge cases that this probably doesn't have correct behavior for *)
let rec actionOfCard = function
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
                                                            |> GameState.gainCard toGain id

  | Militia -> fun id gameState -> GameState.getIdRange gameState
                                    |> Seq.filter (fun x -> x <> id)
                                    |> Seq.fold (fun game id -> GameState.updatePlayer id
                                                                    (fun player -> let card1, card2, card3 = player.militiaReaction player.hand
                                                                                   let drawnDownHand = Utils.withoutNone [card1; card2; card3]
                                                                                                       |> Utils.ensureSubset player.hand
                                                                                                       |> Utils.fillHand player.hand
                                                                                   {player with hand = drawnDownHand}) game)
                                        gameState
  
  | Moneylender -> fun id gameState -> if not (Utils.listMem (GameState.getPlayer id gameState).hand (Coin Copper))
                                       then gameState
                                       else gameState
                                                |> GameState.updatePlayer id
                                                    (fun player -> {player with hand = Utils.withoutFirst ((=) (Coin Copper)) player.hand})
                                                |> GameState.addPurchasingPower MONEYLENDER_PURCHASING_POWER

  | Remodel (toRemodel, toGain) -> fun id gameState -> if not (cardCost toRemodel + 2 >= cardCost toGain
                                                        && Utils.listMem (GameState.getPlayer id gameState).hand toRemodel)
                                                       then gameState
                                                       else GameState.trash toRemodel id gameState
                                                            |> GameState.gainCard toGain id 
  | ThroneRoom act -> fun id gameState -> if (GameState.getPlayer id gameState).hand |> Utils.contains (Action act) |> not
                                          then gameState
                                          else (* TODO if act itself is a ThroneRoom, you're not allowed to play one card 4 times. *)
                                              let action = (actionOfCard act) id
                                              gameState |> action |> action
                                              |> GameState.discard (Action act) id

  | CouncilRoom -> fun aId gameState -> gameState
                                        |> GameState.addBuys COUNCIL_ROOM_BUYS
                                        |> GameState.drawFor COUNCIL_ROOM_SELF_DRAW_COUNT aId
                                        |> Seq.fold (fun game pId -> GameState.drawFor COUNCIL_ROOM_OTHER_DRAW_COUNT pId game)
                                        <| (GameState.getIdRange gameState |> Seq.filter ((<>) aId))

  | Festival -> fun aId gameState -> gameState
                                        |> GameState.addActions FESTIVAL_ACTIONS
                                        |> GameState.addBuys FESTIVAL_BUYS
                                        |> GameState.addPurchasingPower FESTIVAL_PURCHASE_POWER

  | Laboratory -> fun aId gameState -> gameState
                                        |> GameState.addActions LAB_ACTIONS
                                        |> GameState.drawFor LAB_DRAW_COUNT aId

  | Market -> fun aId gameState -> gameState
                                    |> GameState.addActions MARKET_ACTIONS
                                    |> GameState.addBuys MARKET_BUYS
                                    |> GameState.addPurchasingPower MARKET_PURCHASING_POWER
                                    |> GameState.drawFor MARKET_CARDS aId

  | Mine toMine -> fun aId gameState -> if Utils.listMem (GameState.getPlayer aId gameState).hand (Coin toMine) |> not
                                        then gameState
                                        else
                                            (* Technically, you can trash a treasure for an equivalent or shittier one, 
                                               but that seems extraordinarily unlikely to be a good idea from the player's
                                               perspective, so I'm just going to not implement it for now. *)
                                            let toGain = match toMine with
                                                         | Copper -> Silver
                                                         | _ -> Gold 
                                            gameState 
                                                |> GameState.trash (Coin toMine) aId
                                                |> GameState.updatePlayer aId (fun player -> {player with hand = (Coin toGain)::player.hand})
                                    
  | _ -> failwith "not impl"
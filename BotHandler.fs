module BotHandler
    
    open Definitions
    open Constants
    
    module Query =
        let allCards gameState playerId = List.nth gameState.players playerId |> Utils.allCards
        let canBuy id gameState card = GameState.totalPurchasingPower id gameState > Constants.cardCost card;
        let canAct id gameState actCard = (GameState.getPlayer id gameState).hand
                                          |> List.filter (function Action a when a = actCard -> true | _ -> false)
                                          |> List.length
                                          |> ((>) 0)

    module GameStateUpdate =
        let apply id gameState = function
            | Act actCard -> if Query.canAct id gameState actCard
                              then (ActionCards.actionOfCard actCard) id gameState
                              else gameState
            | Buy card -> if Query.canBuy id gameState card
                                then GameState.buy id card gameState
                                else gameState

        let updateAll id updates gameState = List.fold (fun gameState update -> apply id gameState update) gameState updates
            

            
module BotHandler
    
    open Definitions
    open Constants
    
    module Query =
        let allCards gameState playerId = List.nth gameState.players playerId |> Utils.allCards
        let canBuy id gameState card = GameState.totalPurchasingPower id gameState > Constants.cardCost card
                                        && gameState.currentTurn.buys > 0
        let canAct id gameState actCard = Utils.contains (Action actCard) (GameState.getPlayer id gameState).hand
                                            && gameState.currentTurn.actions > 0

        let isValid id gameState = function
            | Act actCard -> canAct id gameState actCard
            | Buy card -> canBuy id gameState card

    module GameStateUpdate =
        let act id actCard gameState = (ActionCards.actionOfCard actCard) id gameState 
                                        |> GameState.withTurn {gameState.currentTurn with actions = gameState.currentTurn.actions - 1}
                                        |> GameState.discard (Action actCard) id

        let buy id card gameState = let availableMoney = GameState.totalPurchasingPower id gameState
                                    let cost = cardCost card 
                                    let withPlayer = GameState.updatePlayer id
                                                        (fun player -> {player with discard = card::player.discard}) gameState
                                    {withPlayer with cards = Map.add card ((Map.find card withPlayer.cards) - 1) withPlayer.cards }
                                    |> GameState.withTurn {withPlayer.currentTurn
                                                            with purchasingPower = withPlayer.currentTurn.purchasingPower - cardCost card}

        let apply id gameState = function
            | Act actCard when Query.canAct id gameState actCard -> act id actCard gameState
            | Buy card when Query.canBuy id gameState card-> buy id card gameState
            | _ -> gameState

        let applyFirstValid id updates gameState =
            match List.tryFind (Query.isValid id gameState) updates with
                | Some update -> apply id gameState update
                | None -> gameState
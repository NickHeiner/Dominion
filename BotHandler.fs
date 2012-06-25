﻿module BotHandler
    
    open Definitions
    open Constants
    
    module Query =
        let allCards gameState playerId = List.nth gameState.players playerId |> Utils.allCards
        let canBuy id gameState card = GameState.totalPurchasingPower id gameState > Constants.cardCost card
                                        && gameState.currentTurn.buys > 0

        (* TODO There is action-card specific validation as well *)
        let canAct id gameState actCard = Utils.contains (Action <| Definitions.getRaw actCard) (GameState.getPlayer id gameState).hand
                                            && gameState.currentTurn.actions > 0

    module GameStateUpdate =
        let act pId (actCard : argActCard) gameState =
            let withTurn = GameState.withTurn {gameState.currentTurn with actions = gameState.currentTurn.actions - 1} gameState 
            let afterAction = (ActionCards.actionOfCard actCard) pId withTurn
            afterAction |> GameState.safeDiscard (Action <| Definitions.getRaw actCard) pId

        let buy id card gameState = let availableMoney = GameState.totalPurchasingPower id gameState
                                    let cost = cardCost card 
                                    let withPlayer = GameState.updatePlayer id
                                                        (fun player -> {player with discard = card::player.discard}) gameState
                                    {withPlayer with cards = Map.add card ((Map.find card withPlayer.cards) - 1) withPlayer.cards }
                                    |> GameState.withTurn {withPlayer.currentTurn
                                                            with purchasingPower = withPlayer.currentTurn.purchasingPower - cardCost card}

        let applyFirstValidBuy id buys gameState =
            match List.tryFind (function Buy card -> Query.canBuy id gameState card) buys with
                | Some (Buy card) -> buy id card gameState
                | None -> gameState

        let applyFirstValidAction pId (acts : argActCard list) gameState =
            match List.tryFind (Query.canAct pId gameState) acts with
                | Some argActCard -> act pId argActCard gameState
                | None -> gameState
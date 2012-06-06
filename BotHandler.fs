module BotHandler
    
    open Definitions
    open Constants
    
    module Query =
        let allCards gameState playerId = List.nth gameState.players playerId |> Utils.allCards
        let canBuy id gameState card = GameState.totalPurchasingPower id gameState > Constants.cardCost card
                                        && gameState.currentTurn.buys > 0
        let canAct id gameState actCard = Utils.contains (Action actCard) (GameState.getPlayer id gameState).hand
                                            && gameState.currentTurn.actions > 0

    module GameStateUpdate =
        let act id actCard gameState = GameState.withTurn {gameState.currentTurn with actions = gameState.currentTurn.actions - 1} gameState 
                                        |> (ActionCards.actionOfCard actCard) id
                                        |> GameState.discard (Action actCard) id

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

        let applyFirstValidAction id acts gameState =
            match List.tryFind (function Act actCard -> Query.canAct id gameState actCard) acts with
                | Some (Act actCard) -> act id actCard gameState
                | None -> gameState
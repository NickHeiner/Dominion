module BotHandler
    
    open Definitions
    open Constants

    let evalCond player = function
        |   Always -> true
        |   ExpectedPerHandLessThan (expected, card) -> let allCards = Utils.allCards player
                                                        let occurences = 
                                                            allCards
                                                            |> Utils.countOccurs card
                                                            |> float 
                                                        let total =
                                                            allCards
                                                            |> List.length
                                                            |> float
                                                        total = 0. || expected < occurences / total
        |  CountInDeckLessThan (count, card) -> count < (player |> Utils.allCards |> Utils.countOccurs card)
    
    module Query =
        let allCards gameState playerId = List.nth gameState.players playerId |> Utils.allCards
        let canBuy id gameState card = GameState.totalPurchasingPower id gameState > Constants.cardCost card
                                        && gameState.currentTurn.buys > 0

        (* TODO There is action-card specific validation as well *)
        let canAct id gameState actCard = Utils.contains (Action <| Definitions.getRaw actCard) (GameState.getPlayer id gameState).hand
                                            && gameState.currentTurn.actions > 0

    module GameStateUpdate =
        let act pId (actCard : argActCard) gameState =
            gameState
            |> GameState.withTurn {gameState.currentTurn with actions = gameState.currentTurn.actions - 1}
            |> (ActionCards.actionOfCard actCard) pId
            |> GameState.safeDiscard (Action <| Definitions.getRaw actCard) pId

        let buy pId card gameState = let availableMoney = GameState.totalPurchasingPower pId gameState
                                     let cost = cardCost card 
                                     let withPlayer = GameState.updatePlayer pId
                                                        (fun player -> {player with discard = card::player.discard}) gameState
                                     {withPlayer with cards = Map.add card ((Map.find card withPlayer.cards) - 1) withPlayer.cards }
                                     |> GameState.withTurn {withPlayer.currentTurn
                                                            with purchasingPower = withPlayer.currentTurn.purchasingPower - cardCost card}

        let applyFirstValidBuy pId buys gameState =
            match List.tryFind (function Buy card -> Query.canBuy pId gameState card) buys with
                | Some (Buy card) -> buy pId card gameState
                | None -> gameState

        let applyFirstValidAction pId (acts : argActCard list) gameState =
            match List.tryFind (Query.canAct pId gameState) acts with
                | Some argActCard -> act pId argActCard gameState
                | None -> gameState
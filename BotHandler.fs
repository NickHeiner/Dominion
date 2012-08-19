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
                                                        total = 0. || expected > occurences / total
        |  CountInCardsLessThan (count, card) -> count > (player |> Utils.allCards |> Utils.countOccurs card)
    
    
    let allCards gameState playerId = List.nth gameState.players playerId |> Utils.allCards
    let canBuy pId gameState card = GameState.totalPurchasingPower pId gameState >= Constants.cardCost card
                                    && gameState.currentTurn.buys > 0

    let canAct aId gameState actCard = Utils.contains (Action <| Definitions.getRaw actCard) (GameState.getPlayer aId gameState).hand
                                        && gameState.currentTurn.actions > 0
                                        && ActionCards.isValidUsage aId gameState actCard

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

        let findFirstValidAction pId acts gameState =
            match List.tryFind (fun (cond, actCard) -> evalCond (GameState.getPlayer pId gameState) cond
                                                       && canAct pId gameState actCard) acts with
                | Some (_, argActCard) -> Some argActCard
                | None -> None

        let findFirstValidBuy pId buys gameState = 
            match List.tryFind (function (cond, card) -> evalCond (GameState.getPlayer pId gameState) cond
                                                         && canBuy pId gameState card) buys with
                    | Some (_, card) -> Some card
                    | None -> None
        
        let applyFirstValidBuy pId buys gameState =
            match findFirstValidBuy pId buys gameState with
                | Some card -> buy pId card gameState
                | None -> gameState

        let applyFirstValidAction pId acts gameState =
            match findFirstValidAction pId acts gameState with
                | Some card -> act pId card gameState
                | None -> gameState
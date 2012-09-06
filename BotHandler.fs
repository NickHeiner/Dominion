module BotHandler
    
    open Definitions
    open Constants

    let actionCardsRequired (bots : bot list) =
        bots
        |> List.map (fun (_, _, buys) -> List.map snd buys)
        |> Utils.flatten
        |> List.fold (fun acc -> function
                                    | Action a -> a::acc
                                    | _ -> acc) []
        |> Set.ofList

    let evalCond gameState aId = function
        |   Always -> true
        |   ExpectedPerHandLessThan (expected, card) -> let allCards =
                                                            gameState
                                                            |> GameState.getPlayer aId
                                                            |> Utils.allCards
                                                        let occurences = 
                                                            allCards
                                                            |> Utils.countOccurs card
                                                            |> float 
                                                        let total =
                                                            allCards
                                                            |> List.length
                                                            |> float
                                                        total = 0. || expected > occurences / total
        |  CountInCardsLessThan (count, card) -> count > GameState.cardCount aId card gameState
        |  CardsRemainingLessThan (count, card) -> count > Utils.defaultFind card 0 gameState.cards
        |  MoreOfFirst (moreOf, lessOf) -> GameState.cardCount aId moreOf gameState >= GameState.cardCount aId lessOf gameState
    
    let allCards gameState playerId = List.nth gameState.players playerId |> Utils.allCards
    let canBuy pId gameState card = GameState.totalPurchasingPower pId gameState >= Constants.cardCost card
                                    && gameState.currentTurn.buys > 0
                                    && Utils.defaultFind card 0 gameState.cards > 0

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
                                     let withPlayer = GameState.addCards 1 pId card gameState
                                     GameState.withTurn {withPlayer.currentTurn
                                                            with purchasingPower = withPlayer.currentTurn.purchasingPower - cardCost card
                                                                 buys = withPlayer.currentTurn.buys - 1}
                                                        withPlayer

        (* TODO Why do all these take acts or buys if they're already getting pId and gameState? It's redundant. *)
        let findFirstValidAction pId acts gameState =
            match List.tryFind (fun (cond, actCard) -> evalCond gameState pId cond
                                                       && canAct pId gameState actCard) acts with
                | Some (_, argActCard) -> Some argActCard
                | None -> None

        let findFirstValidBuy pId buys gameState = 
            match List.tryFind (function (cond, card) -> evalCond gameState pId cond
                                                         && canBuy pId gameState card) buys with
                    | Some (_, card) -> Some card
                    | None -> None
        
        let withLogEntry pId gameState preGameState event = {gameState with log = {pId = pId
                                                                                   event = event
                                                                                   round = gameState.roundsPlayed
                                                                                   turn = preGameState.currentTurn
                                                                                   currHand = GameState.getPlayer pId preGameState
                                                                                              |> GameState.getHand}::gameState.log}

        let applyFirstValidBuy pId buys gameState =
            match findFirstValidBuy pId buys gameState with
                | Some card -> withLogEntry pId (buy pId card gameState) gameState <| Buy card
                | None -> withLogEntry pId gameState gameState PassBuy 

        let applyFirstValidAction pId acts gameState =
            match findFirstValidAction pId acts gameState with
                | Some card -> withLogEntry pId (act pId card gameState) gameState <| Act card
                | None -> withLogEntry pId gameState gameState PassAct 


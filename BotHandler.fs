module BotHandler
    
    open Definitions
    open Constants

    module Query =
        type t = {allCards : int -> card list; canBuy : card -> bool; canAct : int -> gameState -> ActCard -> bool}

        let allCards gameState playerId = List.nth gameState.players playerId |> Utils.allCards
        let canBuy id gameState card = GameState.totalPurchasingPower id gameState > Constants.cardCost card;
        let canAct id gameState actCard = (GameState.getPlayer id gameState).hand
                                          |> List.filter (function Action a when a = actCard -> true | _ -> false)
                                          |> List.length
                                          |> ((>) 0)

        let make id gameState = {allCards = allCards gameState;
                                 canBuy = canBuy id gameState;
                                 canAct = fun actCard -> canAct id gameState actCard}

    module Context =
        type t = {buy : card -> t; act : ActCard -> t; query : Query.t}

        let buy card c = c.buy card
        let act card c = c.act card

        let rec make id gameState =
            {query = Query.make id gameState;
             act = fun actCard -> (if canAct id gameState actCard
                                    then (ActionCards.actionOfCard actCard) id gameState
                                    else gameState) |> make id;
             buy = fun card -> (if canBuy id gameState card
                                then GameState.buy id card gameState
                                else gameState) |> make id}


            

            
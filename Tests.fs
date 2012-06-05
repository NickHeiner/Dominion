module Tests

open NUnit.Framework
open FsUnit
open Definitions
open Constants
open BotHandler

let protoGame = Dominion.Game.getInitialState (List.replicate 5 ("Empty", []))
(* 
module ActionTests =
    [<Test>] let ``smithy test`` () =  let id = 0
                                       let hand = List.replicate 5 (Coin Copper)
                                       let deck = List.replicate 4 (Victory Estate)
                                       let players = (protoGame
                                                    |> GameState.updatePlayer id (fun player -> {player with hand = hand; deck = deck})
                                                    |> Context.make id
                                                    |> Context.act Smithy).query.
                                       ((List.nth players id).hand |> Set.ofList)
                                       |> should equal ((hand @ (List.toSeq deck |> Seq.take SMITHY_CARDS_DRAW |> Seq.toList)) |> Set.ofList)
                                       *)

module BotTests =
    let buy toBuy hand game = 
        let id = 0
        (game 
        |> GameState.updatePlayer id (fun player -> {player with hand = hand})
        |> BotHandler.GameStateUpdate.applyFirstValid id [Buy toBuy]
        |> GameState.getPlayer id).discard

    let [<Test>] ``pass bot does nothing`` () = BotHandler.GameStateUpdate.applyFirstValid 0 [] protoGame |> should equal protoGame
    let [<Test>] ``legal buy`` () = 
        let toBuy = Victory Duchy
        buy toBuy [Coin Gold; Coin Gold] protoGame |> should contain toBuy

    let [<Test>] ``illegal buy not enough money`` () = 
        let toBuy = Victory Province
        buy toBuy [Coin Gold; Coin Gold] protoGame |> Utils.contains toBuy |> should be False

    let [<Test>] ``illegal buy not enough buys`` () = 
        let toBuy = Victory Estate
        protoGame
        |> GameState.withTurn {protoGame.currentTurn with buys = 0}
        |> buy toBuy [Coin Gold; Coin Gold]
        |> Utils.contains toBuy |> should be False

    let [<Test>] ``legal action`` () =
        let id = 0
        let deck = [Coin Copper; Victory Estate; Victory Duchy]
        (protoGame 
        |> GameState.updatePlayer id (fun player -> {player with hand = [Action Smithy]; deck = deck})
        |> BotHandler.GameStateUpdate.applyFirstValid id [Act Smithy]
        |> GameState.getPlayer id).hand 
        |> Set.ofList
        |> should equal (Set.ofList deck)

    let [<Test>] ``illegal action doesn't have card`` () =
        let id = 0
        let origHand = [Coin Copper]
        (protoGame 
        |> GameState.updatePlayer id (fun player -> {player with hand = origHand})
        |> BotHandler.GameStateUpdate.applyFirstValid id [Act Smithy]
        |> GameState.getPlayer id).hand 
        |> should equal origHand

    let [<Test>] ``illegal action not enough actions`` () =
        let id = 0
        let origHand = [Action Smithy]
        (protoGame 
        |> GameState.withTurn {protoGame.currentTurn with actions = 0}
        |> GameState.updatePlayer id (fun player -> {player with hand = origHand})
        |> BotHandler.GameStateUpdate.applyFirstValid id [Act Smithy]
        |> GameState.getPlayer id).hand 
        |> should equal origHand

    module GameStateUpdateTests =
        module BuyTests = 
            let preGameState id = protoGame |> GameState.updatePlayer id (fun player -> {player with hand = [Coin Gold]})
            let doBuy id toBuy = BotHandler.GameStateUpdate.buy id toBuy (preGameState id)

            let [<Test>] ``buy updates player discard`` () = let id = 0
                                                             let toBuy = Victory Estate
                                                             let afterBuy = doBuy id toBuy
                                                             (List.nth afterBuy.players id).discard |> List.head |> should equal toBuy

            let [<Test>] ``buy updates card counts`` () = let id = 0
                                                          let toBuy = Victory Estate
                                                          let afterBuy = doBuy id toBuy
                                                          afterBuy.cards |> Map.find toBuy |> should equal
                                                             ((GameState.initialGameState.cards |> Map.find toBuy) - 1)

            let [<Test>] ``buy lowers purchasing power`` () = let id = 0
                                                              let toBuy = Victory Estate
                                                              let afterBuy = doBuy id toBuy
                                                              GameState.totalPurchasingPower id afterBuy
                                                                |> should equal
                                                                    ((GameState.totalPurchasingPower id (preGameState id))
                                                                        - Constants.cardCost toBuy)
    
module UtilTests =
    let [<Test>] simpleWithNth () = Utils.withNth [5; 3; 2] 0 6 |> should equal [6; 3; 2]
    let [<Test>] negIndex () = (fun () -> Utils.withNth [5; 3; 2] -1 6 |> ignore) |> should throw typeof<System.ArgumentException>
    let [<Test>] tooBigIndex () = (fun () -> Utils.withNth [5; 3; 2] 100 46 |> ignore) |> should throw typeof<System.ArgumentException>
    let [<Test>] lastIndex () = Utils.withNth ["foo"; "bar"; "Baz"] 2 "grumbles" |> should equal ["foo"; "bar"; "grumbles"]
    
module GameStateTests =
    let [<Test>] discard () = let id = 0
                              let toDiscard = Victory Estate
                              (protoGame
                                |> GameState.updatePlayer id (fun player -> {player with hand = [toDiscard]})
                                |> GameState.discard toDiscard id 
                                |> GameState.getPlayer id).hand |> should equal []

    let [<Test>] ``next turn`` () = (GameState.initialGameState
                                    |> GameState.withTurn {GameState.initialGameState.currentTurn with purchasingPower = 3}
                                    |> GameState.nextTurn).currentTurn |> should equal GameState.initialGameState.currentTurn

    module DiscardAllTests =
        let [<Test>] ``simple discard all`` () = let hand = [Victory Curse; Victory Estate; Victory Province]
                                                 let discard = [Coin Copper; Action Smithy]
                                                 let afterDiscard =
                                                    GameState.discardAll {Constants.initialPlayer with hand=hand; discard=discard}
                                                 afterDiscard.hand |> should equal []
                                                 afterDiscard.discard |> should equal (hand @ discard)

    let [<Test>] simpleDraw () = let deck = [Victory Estate; Victory Province; Coin Copper; Coin Silver; Coin Gold]
                                 let afterDraw = GameState.draw 5 {hand=[]; discard=[]; deck=deck; bot=[]}
                                 afterDraw.hand |> Set.ofList |> should equal (Set.ofList deck)
                                 afterDraw.deck |> should equal []

    let [<Test>] biggerDeck () = let deck = [Victory Estate; Victory Province; Coin Copper; Coin Silver; 
                                                Coin Gold; Victory Estate; Coin Silver; Coin Silver]
                                 let drawAmount = 5
                                 let afterDraw = GameState.draw drawAmount {hand=[]; discard=[]; deck=deck; bot=[]}
                                 afterDraw.hand |> List.length |> should equal drawAmount
                                 afterDraw.hand |> Set.ofList |> should equal (deck |> List.toSeq |> Seq.take drawAmount |> Set.ofSeq)
                                 afterDraw.deck |> should equal (deck |> List.toSeq |> Seq.skip drawAmount |> Seq.toList)

    let [<Test>] smallerThanDeck () = let deck = [Victory Estate]
                                      let discard = List.replicate 10 (Coin Copper)
                                      let drawAmount = 5
                                      let afterDraw = GameState.draw drawAmount {hand=[]; discard=discard; deck=deck; bot=[]}
                                      afterDraw.hand |> List.length |> should equal drawAmount
                                      afterDraw.discard |> should equal []

module GameTests =
    let [<Test>] ``get initial bots`` () =
        let bot = []
        (Dominion.Game.getInitialState [("Foo", bot)]).players |> List.length |> should equal 1

    module ScorePlayerTests =
        let [<Test>] ``simple score`` () = Dominion.Game.score 
                                            {Constants.initialPlayer with hand=[Victory Estate];
                                                                            discard=[Victory Duchy; Coin Copper];
                                                                            deck=[Victory Province; Action Smithy]}
                                            |> should equal
                                                (List.sumBy Constants.victoryPointsFor [Victory Estate; Victory Duchy; Victory Province])

        let [<Test>] ``curse score`` () = Dominion.Game.score 
                                            {Constants.initialPlayer with hand=[Victory Estate; Victory Curse];
                                                                            discard=[Victory Duchy; Coin Copper];
                                                                            deck=[Victory Province; Action Smithy]}
                                            |> should equal
                                                (List.sumBy Constants.victoryPointsFor
                                                [Victory Estate; Victory Duchy; Victory Curse; Victory Province])

        let [<Test>] ``gardens score`` () = Dominion.Game.score 
                                                {Constants.initialPlayer with hand=[]; discard=[Victory Gardens]; 
                                                                              deck=List.replicate 43 (Coin Copper)}
                                            |> should equal 4

    module GameOverTests =
        let [<Test>] ``game not over initially`` () = Dominion.Game.gameOver GameState.initialGameState |> should be False
    
        let [<Test>] ``game over after turn limit`` () = Dominion.Game.gameOver
                                                            {GameState.initialGameState with turnsTaken = Constants.TURN_LIMIT + 1}
                                                            |> should be True

        let [<Test>] ``game over when provinces gone`` () = Dominion.Game.gameOver
                                                                {GameState.initialGameState with
                                                                    cards = Map.add (Victory Province) 0 GameState.initialGameState.cards}
                                                            |> should be True

        let [<Test>] ``game over when three cards gone`` () = let cards = GameState.initialGameState.cards 
                                                                            |> Map.add (Victory Estate) 0
                                                                            |> Map.add (Coin Copper) 0
                                                                            |> Map.add (Action Smithy) 0
                                                              Dominion.Game.gameOver {GameState.initialGameState with cards = cards}
                                                            |> should be True

        let [<Test>] ``game not over when two cards gone`` () = let cards = GameState.initialGameState.cards 
                                                                            |> Map.add (Victory Estate) 0
                                                                            |> Map.add (Action Smithy) 0
                                                                Dominion.Game.gameOver {GameState.initialGameState with cards = cards}
                                                                |> should be False
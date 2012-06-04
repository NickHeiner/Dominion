module Tests

open NUnit.Framework
open FsUnit
open Definitions
open BotHandler

let protoGame = Dominion.Game.getInitialState (List.replicate 5 ("Empty", (fun _ x -> x)))

module ActionTests =
    [<Test>] let ``smithy test`` () =  let id = 0
                                       let hand = List.replicate 5 (Coin Copper)
                                       let deck = List.replicate 4 (Victory Estate)
                                       let players = (protoGame
                                                    |> GameState.updatePlayer id (fun player -> {player with hand = hand; deck = deck})
                                                    |> Context.make id
                                                    |> Context.act Smithy).query.
                                       ((List.nth players id).hand |> Set.ofList)
                                       |> should equal ((hand @ (List.toSeq deck |> Seq.take 3 |> Seq.toList)) |> Set.ofList)

module UtilTests =
    [<Test>] let simpleWithNth () = Utils.withNth [5; 3; 2] 0 6 |> should equal [6; 3; 2]
    [<Test>] let negIndex () = (fun () -> Utils.withNth [5; 3; 2] -1 6 |> ignore) |> should throw typeof<System.ArgumentException>
    [<Test>] let tooBigIndex () = (fun () -> Utils.withNth [5; 3; 2] 100 46 |> ignore) |> should throw typeof<System.ArgumentException>
    [<Test>] let lastIndex () = Utils.withNth ["foo"; "bar"; "Baz"] 2 "grumbles" |> should equal ["foo"; "bar"; "grumbles"]
    
module GameStateTests =
    [<Test>] let ``next turn`` () = (GameState.initialGameState
                                    |> GameState.withTurn {GameState.initialGameState.currentTurn with purchasingPower = 3}
                                    |> GameState.nextTurn).currentTurn |> should equal GameState.initialGameState.currentTurn

    module BuyTests = 
        let preGameState id = protoGame |> GameState.updatePlayer id (fun player -> {player with hand = [Coin Gold]})
        let doBuy id toBuy = GameState.buy id toBuy (preGameState id)

        [<Test>] let ``buy updates player discard`` () = let id = 0
                                                         let toBuy = Victory Estate
                                                         let afterBuy = doBuy id toBuy
                                                         (List.nth afterBuy.players id).discard |> List.head |> should equal toBuy

        [<Test>] let ``buy updates card counts`` () = let id = 0
                                                      let toBuy = Victory Estate
                                                      let afterBuy = doBuy id toBuy
                                                      afterBuy.cards |> Map.find toBuy |> should equal
                                                         ((GameState.initialGameState.cards |> Map.find toBuy) - 1)

        [<Test>] let ``buy lowers purchasing power`` () = let id = 0
                                                          let toBuy = Victory Estate
                                                          let afterBuy = doBuy id toBuy
                                                          GameState.totalPurchasingPower id afterBuy
                                                            |> should equal
                                                                ((GameState.totalPurchasingPower id (preGameState id))
                                                                    - Constants.cardCost toBuy)
                                         
    
    module DiscardAllTests =
        [<Test>] let ``simple discard all`` () = let hand = [Victory Curse; Victory Estate; Victory Province]
                                                 let discard = [Coin Copper; Action Smithy]
                                                 let afterDiscard =
                                                    GameState.discardAll {Constants.initialPlayer with hand=hand; discard=discard}
                                                 afterDiscard.hand |> should equal []
                                                 afterDiscard.discard |> should equal (hand @ discard)

    [<Test>] let simpleDraw () = let deck = [Victory Estate; Victory Province; Coin Copper; Coin Silver; Coin Gold]
                                 let afterDraw = GameState.draw 5 {hand=[]; discard=[]; deck=deck; bot=(fun _ x -> x)}
                                 afterDraw.hand |> Set.ofList |> should equal (Set.ofList deck)
                                 afterDraw.deck |> should equal []

    [<Test>] let biggerDeck () = let deck = [Victory Estate; Victory Province; Coin Copper; Coin Silver; 
                                                Coin Gold; Victory Estate; Coin Silver; Coin Silver]
                                 let drawAmount = 5
                                 let afterDraw = GameState.draw drawAmount {hand=[]; discard=[]; deck=deck; bot=(fun _ x -> x)}
                                 afterDraw.hand |> List.length |> should equal drawAmount
                                 afterDraw.hand |> Set.ofList |> should equal (deck |> List.toSeq |> Seq.take drawAmount |> Set.ofSeq)
                                 afterDraw.deck |> should equal (deck |> List.toSeq |> Seq.skip drawAmount |> Seq.toList)

    [<Test>] let smallerThanDeck () = let deck = [Victory Estate]
                                      let discard = List.replicate 10 (Coin Copper)
                                      let drawAmount = 5
                                      let afterDraw = GameState.draw drawAmount {hand=[]; discard=discard; deck=deck; bot=(fun _ x -> x)}
                                      afterDraw.hand |> List.length |> should equal drawAmount
                                      afterDraw.discard |> should equal []

module GameTests =
    [<Test>]
    let ``get initial bots`` () =
        let bot = (fun _ x -> x)
        (Dominion.Game.getInitialState [("Foo", bot)]).players |> List.length |> should equal 1

    module ScorePlayerTests =
        [<Test>] let ``simple score`` () = Dominion.Game.score 
                                            {Constants.initialPlayer with hand=[Victory Estate];
                                                                            discard=[Victory Duchy; Coin Copper];
                                                                            deck=[Victory Province; Action Smithy]}
                                            |> should equal
                                                (List.sumBy Constants.victoryPointsFor [Victory Estate; Victory Duchy; Victory Province])

        [<Test>] let ``curse score`` () = Dominion.Game.score 
                                            {Constants.initialPlayer with hand=[Victory Estate; Victory Curse];
                                                                            discard=[Victory Duchy; Coin Copper];
                                                                            deck=[Victory Province; Action Smithy]}
                                            |> should equal
                                                (List.sumBy Constants.victoryPointsFor
                                                [Victory Estate; Victory Duchy; Victory Curse; Victory Province])

        [<Test>] let ``gardens score`` () = Dominion.Game.score 
                                                {Constants.initialPlayer with hand=[]; discard=[Victory Gardens]; 
                                                                              deck=List.replicate 43 (Coin Copper)}
                                            |> should equal 4

    module GameOverTests =
        [<Test>] let ``game not over initially`` () = Dominion.Game.gameOver GameState.initialGameState |> should be False
    
        [<Test>] let ``game over after turn limit`` () = Dominion.Game.gameOver
                                                            {GameState.initialGameState with turnsTaken = Constants.TURN_LIMIT + 1}
                                                            |> should be True

        [<Test>] let ``game over when provinces gone`` () = Dominion.Game.gameOver
                                                                {GameState.initialGameState with
                                                                    cards = Map.add (Victory Province) 0 GameState.initialGameState.cards}
                                                            |> should be True

        [<Test>] let ``game over when three cards gone`` () = let cards = GameState.initialGameState.cards 
                                                                            |> Map.add (Victory Estate) 0
                                                                            |> Map.add (Coin Copper) 0
                                                                            |> Map.add (Action Smithy) 0
                                                              Dominion.Game.gameOver {GameState.initialGameState with cards = cards}
                                                            |> should be True

        [<Test>] let ``game not over when two cards gone`` () = let cards = GameState.initialGameState.cards 
                                                                            |> Map.add (Victory Estate) 0
                                                                            |> Map.add (Action Smithy) 0
                                                                Dominion.Game.gameOver {GameState.initialGameState with cards = cards}
                                                                |> should be False
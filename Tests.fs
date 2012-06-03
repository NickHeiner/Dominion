module Tests

open NUnit.Framework
open FsUnit
open Definitions

module UtilTests =
    [<Test>] let simpleWithNth ()= Utils.withNth [5; 3; 2] 0 6 |> should equal [6; 3; 2]
    [<Test>] let negIndex () = (fun () -> Utils.withNth [5; 3; 2] -1 6 |> ignore) |> should throw typeof<System.ArgumentException>
    [<Test>] let tooBigIndex () = (fun () -> Utils.withNth [5; 3; 2] 100 46 |> ignore) |> should throw typeof<System.ArgumentException>
    [<Test>] let lastIndex () = Utils.withNth ["foo"; "bar"; "Baz"] 2 "grumbles" |> should equal ["foo"; "bar"; "grumbles"]
    
module GameStateTests =
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
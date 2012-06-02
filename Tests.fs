module Tests

open NUnit.Framework
open FsUnit
open Definitions

    module UtilTests =
        [<TestFixture>] 
        type withNthTest ()=
           [<Test>] member test.
            simpleWithNth ()=
                   Utils.withNth [5; 3; 2] 0 6 |> should equal [6; 3; 2]

            [<Test>] member test.
            negIndex ()=
                   (fun () -> Utils.withNth [5; 3; 2] -1 6 |> ignore) |> should throw typeof<System.ArgumentException>

            [<Test>] member test.
            tooBigIndex ()=
                   (fun () -> Utils.withNth [5; 3; 2] 100 46 |> ignore) |> should throw typeof<System.ArgumentException>

            [<Test>] member test.
            lastIndex ()=
                   Utils.withNth ["foo"; "bar"; "Baz"] 2 "grumbles" |> should equal ["foo"; "bar"; "grumbles"]

    module GameStateTests =
        [<TestFixture>]
        type drawTest ()= 
            [<Test>] member test.simpleDraw ()=
                                            let deck = [Victory Estate; Victory Province; Coin Copper; Coin Silver; Coin Gold]
                                            let afterDraw = GameState.draw 5 {hand=[]; discard=[]; deck=deck; bot=(fun _ x -> x)}
                                            afterDraw.hand |> Set.ofList |> should equal (Set.ofList deck)
                                            afterDraw.deck |> should equal []

            [<Test>] member test.biggerDeck ()=
                                            let deck = [Victory Estate; Victory Province; Coin Copper;
                                                Coin Silver; Coin Gold; Victory Estate; Coin Silver; Coin Silver]
                                            let drawAmount = 5
                                            let afterDraw = GameState.draw drawAmount {hand=[]; discard=[]; deck=deck; bot=(fun _ x -> x)}
                                            afterDraw.hand |> List.length |> should equal drawAmount
                                            afterDraw.hand |> Set.ofList |> should equal (deck |> List.toSeq |> Seq.take drawAmount |> Set.ofSeq)
                                            afterDraw.deck |> should equal (deck |> List.toSeq |> Seq.skip drawAmount |> Seq.toList)

            [<Test>] member test.smallerThanDeck ()=
                                            let deck = [Victory Estate]
                                            let discard = List.replicate 10 (Coin Copper)
                                            let drawAmount = 5
                                            let afterDraw = GameState.draw drawAmount {hand=[]; discard=discard; deck=deck; bot=(fun _ x -> x)}
                                            afterDraw.hand |> List.length |> should equal drawAmount
                                            afterDraw.discard |> should equal []
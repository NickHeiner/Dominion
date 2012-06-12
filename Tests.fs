module Tests

open NUnit.Framework
open FsUnit
open Definitions
open Constants
open BotHandler

let protoGame = Dominion.Game.getInitialState (List.replicate 5 ("Empty", ([], [])))

let memberEquals items1 items2 = List.sort items1 |> should equal <| List.sort items2

module ActionTests =
    let withCard id card = GameState.updatePlayer id (fun player -> {player with hand = card::player.hand})
    let withActionCard id card = withCard id (Action card)
    let useAction id card = protoGame |> withActionCard id card |> GameStateUpdate.act id card
    let countCards id game card = Utils.countOccurences (GameState.getPlayer id game).hand card

    let [<Test>] ``cellar no discard`` () = let id = 0
                                            let cellar = Cellar []
                                            (protoGame
                                            |> withActionCard id cellar
                                            |> BotHandler.GameStateUpdate.act id cellar).currentTurn.actions |> should equal 1

    let [<Test>] cellar () = let id = 0
                             let toDiscard = [Coin Copper; Coin Copper; Victory Duchy]
                             let toKeep = [Victory Province; Victory Estate; Coin Copper]
                             let deck = [Action Smithy; Action Village; Action Smithy]
                             let cellar = Cellar toDiscard
                             let player = protoGame
                                             |> GameState.updatePlayer id
                                                (fun player -> {player with hand = (Action cellar)::toKeep @ toDiscard; deck = deck})
                                             |> BotHandler.GameStateUpdate.act id cellar
                                             |> GameState.getPlayer id
                             (Set.ofList player.hand) |> should equal (Set.ofList (toKeep @ deck))
                             player.deck |> should equal []

    let [<Test>] chapel () = let id = 0
                             let chapel = Chapel (Some (Action Smithy), Some (Coin Copper), Some (Victory Estate), None)
                             let toKeep = [Victory Province; Victory Duchy]
                             let hand = toKeep @ [Action Smithy; Coin Copper; Victory Estate]
                             (protoGame
                             |> GameState.updatePlayer id (fun player -> {player with hand = (Action chapel)::hand})
                             |> BotHandler.GameStateUpdate.act id chapel
                             |> GameState.getPlayer id).hand
                             |> Set.ofList
                             |> should equal toKeep

    let [<Test>] chancellor () = let id = 0
                                 let deck = [Coin Copper; Coin Gold; Victory Estate]
                                 let chancellor = Chancellor NoReshuffle
                                 let afterAct = protoGame
                                                    |> withActionCard id chancellor
                                                    |> GameState.updatePlayer id (fun player -> {player with deck = deck})
                                                    |> BotHandler.GameStateUpdate.act id chancellor
                                 afterAct.currentTurn.purchasingPower |> should equal CHANCELLOR_PURCHASING_POWER
                                 (GameState.getPlayer id afterAct).deck |> should equal deck

    let [<Test>] ``chancellor reshuffle`` () = let id = 0
                                               let deck = [Coin Copper; Coin Gold; Victory Estate]
                                               let chancellor = Chancellor Reshuffle
                                               let afterAct = protoGame
                                                                    |> withActionCard id chancellor
                                                                    |> GameState.updatePlayer id (fun player -> {player with deck = deck})
                                                                    |> GameStateUpdate.act id chancellor
                                               afterAct.currentTurn.purchasingPower |> should equal CHANCELLOR_PURCHASING_POWER
                                               (GameState.getPlayer id afterAct).deck |> should equal []
                                               (Set.ofList (GameState.getPlayer id afterAct).discard)
                                                |> should equal (Set.ofList ((Action chancellor)::deck))

    let [<Test>] village () = let id = 0
                              let initialHandSize = List.length (GameState.getPlayer id protoGame).hand
                              let afterAction = useAction id Village
                              afterAction.currentTurn.actions |> should equal 1
                              List.length (GameState.getPlayer id afterAction).hand |> should equal (initialHandSize + 1)

    let [<Test>] woodcutter () = let id = 0
                                 let initialPurchasingPower = protoGame.currentTurn.purchasingPower
                                 let initialBuys = protoGame.currentTurn.buys
                                 let afterAction = useAction id Woodcutter
                                 afterAction.currentTurn.purchasingPower |> should equal (initialPurchasingPower + WOODCUTTER_PURCHASING_POWER)
                                 afterAction.currentTurn.buys |> should equal (initialBuys + WOODCUTTER_BUYS)

    let [<Test>] feast () = let id = 0
                            let toGain = Victory Duchy
                            let feast = Feast toGain
                            let player = useAction id feast |> GameState.getPlayer id
                            player.discard |> should contain toGain
                            Utils.allCards player |> should not' (contain feast)
                            
    let militiaInitialCard = Coin Copper
    let militiaGame = GameState.getIdRange protoGame 
                        |> Seq.fold (fun game playerId -> GameState.updatePlayer playerId 
                                                            (fun player -> {player with hand = List.replicate (MILITIA_DRAW_DOWN_COUNT + 2)
                                                                                                militiaInitialCard})
                                                           game) protoGame

    let [<Test>] ``militia default reaction`` () = let actorId = 0
                                                   let initialHandSize = List.length (GameState.getPlayer actorId militiaGame).hand
                                                   let afterAction = militiaGame
                                                                        |> withActionCard actorId Militia
                                                                        |> GameStateUpdate.act actorId Militia
                                                   match afterAction.players with
                                                   | hd::tl -> List.length hd.hand |> should equal initialHandSize
                                                               List.iter
                                                                (fun player -> List.length player.hand
                                                                               |> should equal MILITIA_DRAW_DOWN_COUNT) tl
                                                   | [] -> failwith "players should be a non-empty list"
                                                   
    let [<Test>] ``militia too few cards returned`` () = 
        let actorId = 0
        let targetId = 1
        (militiaGame 
        |> GameState.updatePlayer targetId
            (fun player -> {player with militiaReaction = (fun _ -> (Some <| militiaInitialCard, None, None))})
        |> withActionCard actorId Militia
        |> GameStateUpdate.act actorId Militia
        |> GameState.getPlayer targetId).hand
        |> should equal (List.replicate MILITIA_DRAW_DOWN_COUNT militiaInitialCard)

    let [<Test>] ``militia illegal cards returned`` () =
        let actorId = 0
        let targetId = 1
        (militiaGame 
        |> GameState.updatePlayer targetId
            (fun player -> {player with militiaReaction = (fun _ -> (Some (Victory Province), Some (Victory Province), Some (Victory Province)))})
        |> withActionCard actorId Militia
        |> GameStateUpdate.act actorId Militia
        |> GameState.getPlayer targetId).hand
        |> should equal (List.replicate MILITIA_DRAW_DOWN_COUNT militiaInitialCard)
                                                      

    let [<Test>] ``moneylender trash copper`` () = let id = 1
                                                   let initialCopperCount = countCards id protoGame (Coin Copper) + 1
                                                   let afterAction = (protoGame 
                                                                        |> withCard id (Coin Copper)
                                                                        |> withActionCard id Moneylender
                                                                        |> GameStateUpdate.act id Moneylender)
                                                   countCards id afterAction (Coin Copper) |> should equal (initialCopperCount - 1)
                               
    let [<Test>] ``moneylender purchasing power`` () = let id = 1
                                                       let initialPurchasingPower = protoGame.currentTurn.purchasingPower
                                                       let afterAction = (protoGame 
                                                                                |> withCard id (Coin Copper)
                                                                                |> withActionCard id Moneylender
                                                                                |> GameStateUpdate.act id Moneylender)
                                                       afterAction.currentTurn.purchasingPower
                                                        |> should equal (initialPurchasingPower + MONEYLENDER_PURCHASING_POWER)
                               
    let [<Test>] ``moneylender no copper`` () = let id = 1
                                                let initialPurchasingPower = protoGame.currentTurn.purchasingPower
                                                let afterAction = useAction id Moneylender
                                                afterAction.currentTurn.purchasingPower
                                                        |> should equal (initialPurchasingPower)

    let [<Test>] remodel () = let id = 1
                              let toRemodel = Victory Estate
                              let toGain = Action Militia
                              let remodel = Remodel (toRemodel, toGain)
                              let initialToRemodelCount = countCards id protoGame toRemodel + 1
                              let afterAction = protoGame
                                                  |> withCard id toRemodel
                                                  |> GameStateUpdate.act id remodel
                              countCards id afterAction toRemodel |> should equal (initialToRemodelCount - 1)
                              (GameState.getPlayer id afterAction).discard |> should contain toGain
                              
    let [<Test>] ``remodel don't have card to remodel`` () = let id = 1
                                                             let toRemodel = Victory Curse
                                                             let toGain = Victory Estate
                                                             let remodel = Remodel (toRemodel, toGain)
                                                             let afterAction = protoGame
                                                                                |> GameState.updatePlayer id   
                                                                                    (fun player -> 
                                                                                        {player with hand = List.filter ((<>) toRemodel)
                                                                                                                player.hand})
                                                                                |> GameStateUpdate.act id remodel
                                                                                |> GameState.getPlayer id
                                                             afterAction.discard |> should not' (contain toGain)

    let [<Test>] ``remodel toGain to expensive`` () = let id = 1
                                                      let toRemodel = Victory Curse
                                                      let toGain = Action Adventurer
                                                      let remodel = Remodel (toRemodel, toGain)
                                                      let afterAction = protoGame
                                                                        |> withCard id toRemodel
                                                                        |> GameStateUpdate.act id remodel 
                                                                        |> GameState.getPlayer id
                                                      afterAction.hand |> should contain toRemodel
                                                      afterAction.discard |> should not' (contain toGain)

    let [<Test>] ``smithy test`` () =  let id = 0
                                       let hand = List.replicate 5 (Coin Copper)
                                       let deck = List.replicate 4 (Victory Estate)
                                       (protoGame
                                       |> GameState.updatePlayer id (fun player -> {player with hand = (Action Smithy)::hand; deck = deck})
                                       |> BotHandler.GameStateUpdate.act id Smithy
                                       |> GameState.getPlayer id).hand
                                       |> Set.ofList
                                       |> should equal ((hand @ (List.toSeq deck |> Seq.take SMITHY_CARDS_DRAW |> Seq.toList)) |> Set.ofList)

module BotTests =
    let buy toBuy hand game = 
        let id = 0
        (game 
        |> GameState.updatePlayer id (fun player -> {player with hand = hand})
        |> BotHandler.GameStateUpdate.applyFirstValidBuy id [Buy toBuy]
        |> GameState.getPlayer id).discard

    let [<Test>] ``pass bot does nothing`` () = 
        BotHandler.GameStateUpdate.applyFirstValidBuy 0 [] protoGame 
        |> BotHandler.GameStateUpdate.applyFirstValidAction 0 []
        |> should equal protoGame

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
        |> BotHandler.GameStateUpdate.applyFirstValidAction id [Act Smithy]
        |> GameState.getPlayer id).hand 
        |> Set.ofList
        |> should equal (Set.ofList deck)

    let [<Test>] ``illegal action doesn't have card`` () =
        let id = 0
        let origHand = [Coin Copper]
        (protoGame 
        |> GameState.updatePlayer id (fun player -> {player with hand = origHand})
        |> BotHandler.GameStateUpdate.applyFirstValidAction id [Act Smithy]
        |> GameState.getPlayer id).hand 
        |> should equal origHand

    let [<Test>] ``illegal action not enough actions`` () =
        let id = 0
        let origHand = [Action Smithy]
        (protoGame 
        |> GameState.withTurn {protoGame.currentTurn with actions = 0}
        |> GameState.updatePlayer id (fun player -> {player with hand = origHand})
        |> BotHandler.GameStateUpdate.applyFirstValidAction id [Act Smithy]
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
    let [<Test>] fillHand () = let source = {0 .. 10}
                               Utils.fillHand (List.ofSeq source) [] |> memberEquals (Seq.take 3 source |> List.ofSeq)
    let [<Test>] ``fillHand smaller source`` () = let source = {0 .. 2}
                                                  Utils.fillHand (List.ofSeq source) [] |> memberEquals (List.ofSeq source)
    let [<Test>] ``fillHand overlap`` () = let source = [0; 1; 2; 3; 4]
                                           [0] |> Utils.fillHand source |> memberEquals [0; 1; 2]
    let [<Test>] ``fillHand already full`` () = let source = [0; 1; 2; 3; 4; 5]
                                                let newHand = [3; 5; 4]
                                                Utils.fillHand source newHand |> memberEquals newHand

    let [<Test>] ensureSubset () = Utils.ensureSubset [0; 0; 0] [1; 2; 3] |> memberEquals []
    let [<Test>] ``ensureSubset multiple copies`` () = Utils.ensureSubset [0; 0; 4; 5] [1; 0; 1; 0; 0; 0] |> memberEquals [0; 0]
    let [<Test>] ``ensureSubset same`` () = let items = [4; 6; 2; 34; 1]
                                            Utils.ensureSubset items items |> memberEquals items
    
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
                                 let afterDraw = GameState.draw 5 {initialPlayer with deck = deck}
                                 afterDraw.hand |> Set.ofList |> should equal (Set.ofList deck)
                                 afterDraw.deck |> should equal []

    let [<Test>] biggerDeck () = let deck = [Victory Estate; Victory Province; Coin Copper; Coin Silver; 
                                                Coin Gold; Victory Estate; Coin Silver; Coin Silver]
                                 let drawAmount = 5
                                 let afterDraw = GameState.draw drawAmount {initialPlayer with deck = deck}
                                 afterDraw.hand |> List.length |> should equal drawAmount
                                 afterDraw.hand |> Set.ofList |> should equal (deck |> List.toSeq |> Seq.take drawAmount |> Set.ofSeq)
                                 afterDraw.deck |> should equal (deck |> List.toSeq |> Seq.skip drawAmount |> Seq.toList)

    let [<Test>] smallerThanDeck () = let deck = [Victory Estate]
                                      let discard = List.replicate 10 (Coin Copper)
                                      let drawAmount = 5
                                      let afterDraw = GameState.draw drawAmount {initialPlayer with deck = deck; discard = discard}
                                      afterDraw.hand |> List.length |> should equal drawAmount
                                      afterDraw.discard |> should equal []

module GameTests =
    let [<Test>] ``get initial bots`` () =
        let bot = [], []
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
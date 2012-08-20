module Tests

open NUnit.Framework
open FsUnit
open Definitions
open Constants
open BotHandler

let protoGame = Dominion.Game.getInitialState (List.replicate 5 ("Empty", [], []))
                |> GameState.withCards STARTING_CARDS

let memberEquals items1 items2 = List.sort items1 |> should equal <| List.sort items2

let withCard pId card = GameState.updatePlayer pId (fun player -> {player with hand = card::player.hand})
let withActionCard pId card = withCard pId (Action card)

module ActionTests =
    let useAction pId card = protoGame |> withActionCard pId (Definitions.getRaw card) |> GameStateUpdate.act pId card
    let countCards pId game card = Utils.countOccurences (GameState.getPlayer pId game |> GameState.getHand) card

    let [<Test>] bureaucrat () = 
        let aId = PId 1
        let victory = Victory Province
        protoGame
        |> GameState.foldPlayers (fun pId player -> if pId = aId
                                                    then {player with hand=[Action Bureaucrat]; deck=[Victory Gardens]}
                                                    else {player with hand=[victory]; deck=[Coin Copper]})
        |> GameStateUpdate.act aId ABureaucrat
        |> GameState.getPlayers
        |> Utils.withIndices
        |> List.iter (fun (pId, player) -> List.head player.deck |> should equal (if PId pId = aId then BUREAUCRAT_CARD_GAIN else victory))

    let bureaucratTest preHand =
        let aId = PId 1
        protoGame
        |> GameState.foldPlayers (fun pId player -> if pId = aId
                                                    then {player with hand=[Action Bureaucrat]; deck=[Victory Gardens]}
                                                    else {player with hand=preHand; deck=[Coin Copper]})
        |> GameStateUpdate.act aId ABureaucrat
        |> GameState.getPlayers
        |> Utils.withIndices
        |> List.iter (fun (pId, player) -> if PId pId <> aId then player.hand |> memberEquals preHand)
    
    let [<Test>] ``bureaucrat blocked by moat`` () = bureaucratTest [Victory Gardens; Action Moat]
    let [<Test>] ``bureaucrat defender no victory`` () = bureaucratTest [Coin Copper; Coin Silver]

    let [<Test>] ``cellar no discard`` () = let pId = PId 0
                                            let cellar = ACellar []
                                            (protoGame
                                            |> withActionCard pId Cellar
                                            |> BotHandler.GameStateUpdate.act pId cellar).currentTurn.actions |> should equal 1

    let [<Test>] cellar () = let id = PId 0
                             let toDiscard = [Coin Copper; Coin Copper; Victory Duchy]
                             let toKeep = [Victory Province; Victory Estate; Coin Copper]
                             let deck = [Action Smithy; Action Village; Action Smithy]
                             let cellar = ACellar toDiscard
                             let player = protoGame
                                             |> GameState.updatePlayer id
                                                (fun player -> {player with hand = (Action Cellar)::toKeep @ toDiscard; deck = deck})
                                             |> BotHandler.GameStateUpdate.act id cellar
                                             |> GameState.getPlayer id
                             (Set.ofList player.hand) |> should equal (Set.ofList (toKeep @ deck))
                             player.deck |> should equal []

    let [<Test>] chapel () = let id = PId 0
                             let chapel = AChapel (Some (Action Smithy), Some (Coin Copper), Some (Victory Estate), None)
                             let toKeep = [Victory Province; Victory Duchy]
                             let hand = toKeep @ [Action Smithy; Coin Copper; Victory Estate]
                             (protoGame
                             |> GameState.updatePlayer id (fun player -> {player with hand = (Action Chapel)::hand})
                             |> BotHandler.GameStateUpdate.act id chapel
                             |> GameState.getPlayer id).hand
                             |> Set.ofList
                             |> should equal toKeep

    let [<Test>] chancellor () = let aId = PId 0
                                 let deck = [Coin Copper; Coin Gold; Victory Estate]
                                 let chancellor = AChancellor NoReshuffle
                                 let afterAct = protoGame
                                                    |> withActionCard aId Chancellor
                                                    |> GameState.updatePlayer aId (fun player -> {player with deck = deck})
                                                    |> BotHandler.GameStateUpdate.act aId chancellor
                                 afterAct.currentTurn.purchasingPower |> should equal CHANCELLOR_PURCHASING_POWER
                                 (GameState.getPlayer aId afterAct).deck |> should equal deck

    let [<Test>] ``chancellor reshuffle`` () = let aId = PId 0
                                               let deck = [Coin Copper; Coin Gold; Victory Estate]
                                               let chancellor = AChancellor Reshuffle
                                               let afterAct = protoGame
                                                                    |> withActionCard aId Chancellor
                                                                    |> GameState.updatePlayer aId (fun player -> {player with deck = deck})
                                                                    |> GameStateUpdate.act aId chancellor
                                               afterAct.currentTurn.purchasingPower |> should equal CHANCELLOR_PURCHASING_POWER
                                               (GameState.getPlayer aId afterAct).deck |> should equal []
                                               (Set.ofList (GameState.getPlayer aId afterAct).discard)
                                                |> should equal (Set.ofList ((Action Chancellor)::deck))

    let [<Test>] village () = let id = PId 0
                              let initialHandSize = List.length (GameState.getPlayer id protoGame).hand
                              let afterAction = useAction id AVillage
                              afterAction.currentTurn.actions |> should equal 1
                              List.length (GameState.getPlayer id afterAction).hand |> should equal (initialHandSize + 1)

    let [<Test>] woodcutter () = let id = PId 0
                                 let initialPurchasingPower = protoGame.currentTurn.purchasingPower
                                 let initialBuys = protoGame.currentTurn.buys
                                 let afterAction = useAction id AWoodcutter
                                 afterAction.currentTurn.purchasingPower |> should equal (initialPurchasingPower + WOODCUTTER_PURCHASING_POWER)
                                 afterAction.currentTurn.buys |> should equal (initialBuys + WOODCUTTER_BUYS)

    let [<Test>] feast () = let aId = PId 0
                            let toGain = Victory Duchy
                            let feast = AFeast toGain
                            let player = useAction aId feast |> GameState.getPlayer aId
                            player.discard |> should contain toGain
                            Utils.allCards player |> should not' (contain feast)
                            
    let militiaInitialCard = Coin Copper
    let militiaGame = 
        GameState.getIdRange protoGame 
        |> Seq.fold (fun game playerId -> GameState.updatePlayer
                                            playerId 
                                            (fun player -> {player with hand = List.replicate (MILITIA_DRAW_DOWN_COUNT + 2) militiaInitialCard})
                                            game)
                    protoGame

    let [<Test>] ``militia default reaction`` () =
        let aId = PId 0
        let initialHandSize = List.length (GameState.getPlayer aId militiaGame).hand
        militiaGame
        |> withActionCard aId Militia
        |> GameStateUpdate.act aId AMilitia
        |> GameState.getPlayers       
        |> Utils.withIndices
        |> List.iter (fun (pId, player) -> List.length player.hand |> should equal
                                           <| if PId pId = aId 
                                              then initialHandSize                    
                                              else MILITIA_DRAW_DOWN_COUNT)
                                                   
    let [<Test>] ``militia too few cards returned by defender`` () = 
        let actorId = PId 0
        let targetId = PId 1
        (militiaGame 
        |> GameState.updatePlayer targetId
            (fun player -> {player with militiaReaction = (fun _ -> (Some militiaInitialCard, None, None))})
        |> withActionCard actorId Militia
        |> GameStateUpdate.act actorId AMilitia
        |> GameState.getPlayer targetId).hand
        |> should equal (List.replicate MILITIA_DRAW_DOWN_COUNT militiaInitialCard)

    let [<Test>] ``militia illegal cards returned by defender`` () =
        let actorId = PId 0
        let targetId = PId 1
        let illegalCard = Some <| Victory Province
        militiaGame 
        |> GameState.updatePlayer targetId
            (fun player -> {player with militiaReaction = (fun _ -> illegalCard, illegalCard, illegalCard)})
        |> withActionCard actorId Militia
        |> GameStateUpdate.act actorId AMilitia
        |> GameState.getPlayer targetId
        |> GameState.getHand
        |> should equal (List.replicate MILITIA_DRAW_DOWN_COUNT militiaInitialCard)
        
    let [<Test>] ``militia blocked by moat`` () =
        let aId = PId  1
        let targetId = PId 0
        let game = militiaGame
                    |> GameState.updatePlayer targetId (fun player -> {player with hand=(Action Moat)::player.hand;
                                                                                   militiaReaction = function 
                                                                                                     | a::b::c::_ -> (Some a, Some b, Some c)
                                                                                                     | a::b::[] -> (Some a, Some b, None)
                                                                                                     | a::[] -> (Some a, None, None)
                                                                                                     | [] -> (None, None, None)})
        let origTargetHand = (GameState.getPlayer targetId game).hand
        game
        |> withActionCard aId Militia
        |> GameStateUpdate.act aId AMilitia 
        |> GameState.getPlayer targetId
        |> GameState.getHand
        |> memberEquals origTargetHand

    (* attack card tests verify that they are blocked by the moat *)
    let [<Test>] moat () =
        let aId = PId 0
        let preHandLen = protoGame
                            |> GameState.getPlayer aId
                            |> GameState.getHand
                            |> List.length
        protoGame
        |> withActionCard aId Moat
        |> GameStateUpdate.act aId AMoat
        |> GameState.getPlayer aId
        |> GameState.getHand
        |> List.length
        |> should equal <| preHandLen + MOAT_CARD_COUNT

    let [<Test>] ``moneylender trash copper`` () = let id = PId 1
                                                   let initialCopperCount = countCards id protoGame (Coin Copper) + 1
                                                   let afterAction = (protoGame 
                                                                        |> withCard id (Coin Copper)
                                                                        |> withActionCard id Moneylender
                                                                        |> GameStateUpdate.act id AMoneylender)
                                                   countCards id afterAction (Coin Copper) |> should equal (initialCopperCount - 1)
                               
    let [<Test>] ``moneylender purchasing power`` () = let id = PId 1
                                                       let initialPurchasingPower = protoGame.currentTurn.purchasingPower
                                                       let afterAction = (protoGame 
                                                                                |> withCard id (Coin Copper)
                                                                                |> withActionCard id Moneylender
                                                                                |> GameStateUpdate.act id AMoneylender)
                                                       afterAction.currentTurn.purchasingPower
                                                        |> should equal (initialPurchasingPower + MONEYLENDER_PURCHASING_POWER)
                               
    let [<Test>] ``moneylender no copper`` () = let id = PId 1
                                                let initialPurchasingPower = protoGame.currentTurn.purchasingPower
                                                let afterAction = useAction id AMoneylender
                                                afterAction.currentTurn.purchasingPower
                                                        |> should equal (initialPurchasingPower)

    let [<Test>] remodel () = let id = PId 1
                              let toRemodel = Victory Estate
                              let toGain = Action Militia
                              let remodel = ARemodel (toRemodel, toGain)
                              let initialToRemodelCount = countCards id protoGame toRemodel + 1
                              let afterAction = protoGame
                                                  |> withCard id toRemodel
                                                  |> GameStateUpdate.act id remodel
                              countCards id afterAction toRemodel |> should equal (initialToRemodelCount - 1)
                              (GameState.getPlayer id afterAction).discard |> should contain toGain
                              
    let [<Test>] ``remodel don't have card to remodel`` () = let pId = PId 1
                                                             let toRemodel = Action Smithy
                                                             let toGain = Action Feast
                                                             let remodel = ARemodel (toRemodel, toGain)
                                                             let afterAction = protoGame
                                                                                |> GameState.updatePlayer pId   
                                                                                    (fun player -> 
                                                                                        {player with hand = List.filter ((<>) toRemodel)
                                                                                                                player.hand;
                                                                                                     discard = []})
                                                                                |> GameStateUpdate.act pId remodel 
                                                                                |> GameState.getPlayer pId
                                                             afterAction.discard |> should not' (contain toGain)

    let [<Test>] ``remodel toGain too expensive`` () = let id = PId 1
                                                       let toRemodel = Victory Curse
                                                       let toGain = Action Adventurer
                                                       let remodel = ARemodel (toRemodel, toGain)
                                                       let afterAction = protoGame
                                                                        |> withCard id toRemodel
                                                                        |> GameStateUpdate.act id remodel 
                                                                        |> GameState.getPlayer id
                                                       afterAction.hand |> should contain toRemodel
                                                       afterAction.discard |> should not' (contain toGain)

    let [<Test>] smithy () =  
        let aId = PId 0
        let hand = List.replicate 5 (Coin Copper)
        let deck = List.replicate 4 (Victory Estate)
        protoGame
        |> GameState.updatePlayer aId (fun player -> {player with hand = (Action Smithy)::hand; deck = deck})
        |> BotHandler.GameStateUpdate.act aId ASmithy
        |> GameState.getPlayer aId
        |> GameState.getHand
        |> memberEquals <| hand @ (List.toSeq deck |> Seq.take SMITHY_CARDS_DRAW |> Seq.toList)

    let [<Test>] spy () =
        let aId = PId 0
        let spy = ASpy <| SpyChoice ((function Victory _ -> Discard | _ -> NoDiscard), (function Victory _ -> NoDiscard | _ -> Discard))
        let selfDeckPrefix = [Victory Curse; Coin Copper]
        let selfDeckSuffix = [Victory Estate]
        let otherDeck = [Coin Gold]
        protoGame
        |> GameState.foldPlayers (fun pId player -> (if pId = aId 
                                                     then {player with hand = [Action Spy]; deck = selfDeckPrefix @ selfDeckSuffix; discard = []}
                                                     else {player with deck = otherDeck; discard = []}))
        |> GameStateUpdate.act aId spy
        |> GameState.getPlayers
        |> Utils.withIndices
        |> List.iter (fun (pId, player) -> if PId pId = aId
                                           then 
                                                player.hand |> should equal [List.head selfDeckPrefix]
                                                player.deck |> should equal <| List.tail (selfDeckPrefix @ selfDeckSuffix)
                                           else 
                                                player.deck |> should equal []
                                                player.discard |> should equal otherDeck)

    let [<Test>] ``spy blocked by moat`` () =
        let aId = PId 0
        let tId = PId 1
        let spy = ASpy <| SpyChoice ((function Victory _ -> Discard | _ -> NoDiscard), (function Victory _ -> NoDiscard | _ -> Discard))
        let preGame = protoGame
                        |> GameState.updatePlayer tId
                            (fun player -> {player with hand=(Action Moat)::player.hand; deck=[Coin Silver]; discard=[]}) 
        let prePlayer = GameState.getPlayer tId preGame
        preGame
        |> withActionCard aId Spy
        |> GameStateUpdate.act aId spy
        |> GameState.getPlayer tId
        |> should equal prePlayer
                                                        
    let [<Test>] ``spy deck empty`` () =
        let aId = PId 0
        let spy = ASpy <| SpyChoice ((function Victory _ -> Discard | _ -> NoDiscard), (function Victory _ -> NoDiscard | _ -> Discard))
        let selfDeckPrefix = [Coin Copper]
        let selfDeckSuffix = [Victory Estate]
        let otherDiscard = [Victory Gardens]
        ((Seq.fold (fun game pId -> (if pId = aId 
                                     then GameState.updatePlayer pId
                                            (fun player -> {player with hand = [Action Spy]; deck = selfDeckPrefix @ selfDeckSuffix; discard = []})
                                     else GameState.updatePlayer pId
                                            (fun player -> {player with deck = []; discard = otherDiscard})) game) protoGame
        <| GameState.getIdRange protoGame) 
        |> GameStateUpdate.act aId spy).players
        |> Utils.withIndices
        |> List.iter (fun (pId, player) -> if PId pId = aId
                                           then 
                                                player.hand |> should equal selfDeckPrefix
                                                memberEquals player.discard <| (Action Spy)::selfDeckSuffix
                                                player.deck |> should equal []
                                           else 
                                                player.deck |> should equal []
                                                player.discard |> should equal otherDiscard)
                                                
    let [<Test>] ``spy discard self, keep other`` () =
        let aId = PId 0
        let spy = ASpy <| SpyChoice ((function Victory _ -> Discard | _ -> NoDiscard), (function Victory _ -> NoDiscard | _ -> Discard))
        let selfDeckPrefix = [Victory Estate]
        let selfDeckSuffix = [Action Smithy]
        let otherDeck = [Victory Gardens]
        ((Seq.fold (fun game pId -> (if pId = aId 
                                     then GameState.updatePlayer pId
                                            (fun player -> {player with hand = [Action Spy]; deck = selfDeckPrefix @ selfDeckSuffix; discard = []})
                                     else GameState.updatePlayer pId
                                            (fun player -> {player with deck = otherDeck; discard = []})) game) protoGame
        <| GameState.getIdRange protoGame) 
        |> GameStateUpdate.act aId spy).players
        |> Utils.withIndices
        |> List.iter (fun (pId, player) -> if PId pId = aId
                                           then 
                                                player.hand |> should equal selfDeckPrefix
                                                player.deck |> should equal selfDeckSuffix
                                                player.discard |> should equal [Action Spy]
                                           else 
                                                player.deck |> should equal otherDeck
                                                player.discard |> should equal [])
    
    let thiefTest shouldGain actorDeck targetDeck targetDiscard startHand =
        let aId = PId 0
        let priorities = function
                            | Gold -> First
                            | Silver -> Second
                            | Copper -> Third
        let afterAction = 
            (protoGame
            |> GameState.foldPlayers (fun pId player -> {player with deck = [Coin Copper; Coin Copper]; hand = startHand; discard = []})
            |> GameState.updatePlayer aId (fun player -> {player with hand = [Action Thief]})
            |> GameStateUpdate.act aId (AThief <| ThiefChoice (priorities, shouldGain)))
        afterAction.players
        |> Utils.withIndices
        |> List.iter (fun (pId, player) -> if PId pId = aId 
                                           then
                                            player.discard |> memberEquals <| actorDeck 
                                           else
                                            player.discard |> should equal targetDiscard;
                                            player.deck |> should equal targetDeck;
                                            player.hand |> should equal startHand)
    
    let [<Test>] ``thief should gain`` () = thiefTest (fun _ -> Gain)
                                                      ((Action Thief)::(List.replicate (List.length protoGame.players - 1) (Coin Copper)))
                                                      []
                                                      [Coin Copper]
                                                      []

    let [<Test>] ``thief should not gain`` () = thiefTest (fun _ -> NoGain) [Action Thief] [] [Coin Copper] []

    let [<Test>] ``thief blocked by moat`` () = thiefTest (fun _ -> NoGain) [Action Thief] [Coin Copper; Coin Copper] [] [Action Moat]
        
    let [<Test>] ``thief no treasure`` () = 
        let aId = PId 1
        let priorities = function
                            | Gold -> First
                            | Silver -> Second
                            | Copper -> Third
        let shouldGain coin = Gain
        let preDeckPrefix = [Victory Estate; Action Smithy]
        let preDeckSuffix = [Coin Copper; Coin Gold]
        let preDeck = preDeckPrefix @ preDeckSuffix
        (protoGame
            |> GameState.foldPlayers (fun pId player -> {player with deck = preDeck; hand = []; discard = []})
            |> GameState.updatePlayer aId (fun player -> {player with hand = [Action Thief]})
            |> GameStateUpdate.act aId (AThief <| ThiefChoice (priorities, shouldGain))).players
        |> Utils.withIndices
        |> List.iter (fun (pId, player) -> if PId pId = aId
                                           then
                                                player.discard |> should equal [Action Thief]
                                           else
                                                player.discard |> memberEquals preDeckPrefix
                                                player.deck |> memberEquals preDeckSuffix)
                                    
    let [<Test>] ``thief must shuffle`` () = 
        let aId = PId 0
        let priorities = function
                    | Gold -> Third
                    | Silver -> Second
                    | Copper -> First
        let shouldGain coin = Gain
        (protoGame
            |> GameState.foldPlayers (fun pId player -> {player with deck = []; hand = []; discard = [Coin Copper; Coin Gold]})
            |> GameState.updatePlayer aId (fun player -> {player with hand = [Action Thief]; discard = []})
            |> GameStateUpdate.act aId (AThief <| ThiefChoice (priorities, shouldGain))).players
        |> Utils.withIndices
        |> List.iter (fun (pId, player) -> if PId pId = aId
                                           then List.filter ((<>) (Action Thief)) player.discard
                                                |> memberEquals <| List.replicate (List.length protoGame.players - 1) (Coin Copper)
                                           else player.discard |> should equal [Coin Gold])
                                         
    (* TODO: thief: verify that "A player with just one card left reveals that last card and
             then shuffles to get the other card to reveal (without including the revealed card)" *)
    
    let [<Test>] throneRoom () = 
        let actorId = PId 1
        let hand = List.replicate 5 (Coin Copper)
        let deck = List.replicate 12 (Victory Estate)
        (protoGame
        |> GameState.updatePlayer actorId (fun player -> {player with hand = (Action ThroneRoom)::(Action Smithy)::hand; deck = deck})
        |> BotHandler.GameStateUpdate.act actorId (AThroneRoom ASmithy)
        |> GameState.getPlayer actorId).hand
        |> memberEquals <| (hand @ (List.toSeq deck |> Seq.take (SMITHY_CARDS_DRAW * 2) |> Seq.toList))
        
    let [<Test>] ``throneRoom doesn't have card`` () = 
        let actorId = PId 1
        let hand = List.replicate 5 (Coin Copper)
        let deck = List.replicate 12 (Victory Estate)
        (protoGame
        |> GameState.updatePlayer actorId (fun player -> {player with hand = (Action ThroneRoom)::hand; deck = deck})
        |> BotHandler.GameStateUpdate.act actorId (AThroneRoom ASmithy)
        |> GameState.getPlayer actorId).hand
        |> memberEquals hand

    let [<Test>] councilRoom () =
        let actorId = PId 2
        let hand = List.replicate 5 (Coin Silver)
        let deck = List.replicate 12 (Victory Curse)
        let afterAction =
            GameState.getIdRange protoGame
            |> Seq.fold (fun game pId -> GameState.updatePlayer pId (fun player -> {player with hand = hand; deck = deck}) game) protoGame
            |> withActionCard actorId CouncilRoom
            |> GameStateUpdate.act actorId ACouncilRoom
        afterAction.currentTurn.buys |> should equal <| protoGame.currentTurn.buys + COUNCIL_ROOM_BUYS
        afterAction.players
        |> List.map (fun player -> player.hand)
        |> Utils.withIndices
        |> List.iter (fun (pId, newHand) -> if PId pId = actorId
                                             then memberEquals newHand <| hand @ (deck
                                                                            |> List.toSeq
                                                                            |> Seq.take COUNCIL_ROOM_SELF_DRAW_COUNT
                                                                            |> Seq.toList)
                                             else memberEquals newHand <| hand @ (deck
                                                                            |> List.toSeq
                                                                            |> Seq.take COUNCIL_ROOM_OTHER_DRAW_COUNT
                                                                            |> Seq.toList))

    let [<Test>] festival () =
        let aId = PId 1
        let initialTurn = protoGame.currentTurn
        let afterTurn = (useAction aId AFestival).currentTurn
        afterTurn.actions |> should equal <| initialTurn.actions + FESTIVAL_ACTIONS - 1 (* -1 for use of festival *)
        afterTurn.purchasingPower |> should equal <| initialTurn.purchasingPower + FESTIVAL_PURCHASE_POWER
        afterTurn.buys |> should equal <| initialTurn.buys + FESTIVAL_BUYS

    let [<Test>] laboratory () =
        let aId = PId 0
        let deckCard = Victory Duchy
        let hand = List.replicate 5 (Coin Gold)
        let deck = List.replicate 4 deckCard
        let afterAction = (protoGame
                            |> GameState.updatePlayer aId (fun player -> {player with hand = (Action Laboratory)::hand; deck = deck})
                            |> GameStateUpdate.act aId ALaboratory)
        (GameState.getPlayer aId afterAction).hand |> memberEquals <| hand @ (List.replicate LAB_DRAW_COUNT deckCard)
        afterAction.currentTurn.actions |> should equal <| protoGame.currentTurn.actions + LAB_ACTIONS - 1 (* -1 for use of lab *)

    let [<Test>] market () =
        let aId = PId 1
        let afterAction = useAction aId AMarket 
        afterAction.currentTurn.actions |> should equal <| protoGame.currentTurn.actions + MARKET_ACTIONS - 1 (* -1 for use of action *)
        afterAction.currentTurn.buys |> should equal <| protoGame.currentTurn.buys + MARKET_BUYS
        afterAction.currentTurn.purchasingPower |> should equal <| protoGame.currentTurn.purchasingPower + MARKET_PURCHASING_POWER
        (GameState.getPlayer aId afterAction).hand |> List.length |> should equal
            <| ((GameState.getPlayer aId protoGame).hand |> List.length) + MARKET_CARDS

    let [<Test>] mine () =
        let aId = PId 0
        let mine = AMine Copper
        let afterAction =
            protoGame
            |> GameState.updatePlayer aId (fun player -> {player with hand = [Coin Copper; Action Mine]; deck=[]; discard=[]})
            |> GameStateUpdate.act aId mine
            |> GameState.getPlayer aId
        afterAction.hand |> should equal [Coin Silver]
        Utils.allCards afterAction |> should not' (contain (Coin Copper))

    let [<Test>] ``mine doesn't have treasure`` () =
        let aId = PId 1
        let mine = AMine Silver
        let afterAction =
            protoGame
            |> GameState.updatePlayer aId (fun player -> {player with hand = [Action Mine]; deck=[]; discard=[]})
            |> GameStateUpdate.act aId mine
            |> GameState.getPlayer aId
        afterAction.hand |> should equal []
        Utils.allCards afterAction |> should not' (contain (Coin Gold))

    let [<Test>] witch () =
        let aId = PId 0
        let deck = List.replicate 2 <| Victory Estate
        (protoGame
        |> GameState.updatePlayer aId (fun player -> {player with hand = [Action Witch]; deck = deck})
        |> GameStateUpdate.act aId AWitch).players
        |> Utils.withIndices
        |> List.iter (fun (pId, player) -> if PId pId = aId
                                            then player.hand |> should equal deck
                                            else player.discard |> should contain (Victory Curse))

    let [<Test>] ``witch blocked by moat`` () =
        let aId = PId 0
        let withMoat = PId 1
        let deck = List.replicate 2 <| Victory Estate
        (protoGame
        |> GameState.updatePlayer withMoat (fun player -> {player with hand = [Action Moat]})
        |> GameState.updatePlayer aId (fun player -> {player with hand = [Action Witch]; deck = deck})
        |> GameStateUpdate.act aId AWitch).players
        |> Utils.withIndices
        |> List.iter (fun (pId, player) -> match PId pId with
                                            | x when x = aId ->      player.hand |> should equal deck
                                            | x when x = withMoat -> player.discard |> should not' (contain (Victory Curse))
                                            | _ ->                   player.discard |> should contain (Victory Curse))

    let [<Test>] workshop () = 
        let aId = PId 1
        let toGain = Victory Gardens
        protoGame
        |> GameState.withCards ((Victory Gardens)::STARTING_CARDS)
        |> withActionCard aId Workshop
        |> GameStateUpdate.act aId (AWorkshop toGain)
        |> GameState.getPlayer aId
        |> GameState.getDiscard
        |> should contain toGain

    let [<Test>] ``workshop card too expensive`` () = 
        let aId = PId 1
        let toGain = Victory Province
        protoGame
        |> withActionCard aId Workshop
        |> GameStateUpdate.act aId (AWorkshop toGain)
        |> GameState.getPlayer aId
        |> GameState.getDiscard
        |> should not' (contain toGain)
    
    let [<Test>] adventurer () =
        let aId = PId 1
        let treasure1 = Coin Gold
        let treasure2 = Coin Silver
        let gardens = Victory Gardens
        let deck = (List.replicate 4 <| Victory Estate) @ [treasure1] @ (List.replicate 20 <| Action Smithy)
        let discard = (List.replicate 4 <| Action Market) @ [treasure2]  @ (List.replicate 20 <| Victory Gardens)
        let afterAction = 
            protoGame
            |> GameState.updatePlayer aId (fun player -> {player with hand = [Action Adventurer; gardens]; deck = deck; discard = discard})
            |> GameStateUpdate.act aId AAdventurer
            |> GameState.getPlayer aId
        afterAction.hand |> memberEquals [treasure1; treasure2; gardens]

    let [<Test>] ``adventurer only one treasure`` () =
        let aId = PId 1
        let treasure1 = Coin Gold
        let gardens = Victory Gardens
        let deck = (List.replicate 4 <| Victory Estate) @ [treasure1] @ (List.replicate 20 <| Action Smithy)
        let discard = (List.replicate 4 <| Action Market) @ (List.replicate 20 <| Victory Gardens)
        let afterAction = 
            protoGame
            |> GameState.updatePlayer aId (fun player -> {player with hand = [Action Adventurer; gardens]; deck = deck; discard = discard})
            |> GameStateUpdate.act aId AAdventurer
            |> GameState.getPlayer aId
        afterAction.hand |> memberEquals [treasure1; gardens]

    let [<Test>] ``adventurer no shuffle necessary`` () =
        let aId = PId 1
        let treasure1 = Coin Gold
        let treasure2 = Coin Silver
        let gardens = Victory Gardens
        let deckPrefix = List.replicate 4 <| Victory Estate
        let deckSuffix = List.replicate 20 <| Action Smithy
        let deck = deckPrefix @ [treasure1; treasure2] @ deckSuffix
        let discard = (List.replicate 4 <| Action Market) @ (List.replicate 20 <| Victory Gardens)
        let afterAction = 
            protoGame
            |> GameState.updatePlayer aId (fun player -> {player with hand = [Action Adventurer; gardens]; deck = deck; discard = discard})
            |> GameStateUpdate.act aId AAdventurer
            |> GameState.getPlayer aId
        afterAction.hand |> memberEquals [treasure1; treasure2; gardens]
        afterAction.discard |> memberEquals <| deckPrefix @ discard @ [Action Adventurer]
        afterAction.deck |> should equal deckSuffix

    let [<Test>] ``library discard smithies`` () =
        let aId = PId 0
        let deck = [Coin Copper; Action Smithy; Coin Silver; Action Thief; Coin Gold;]
        let origHand = [Victory Province; Victory Curse; Victory Duchy; Victory Estate]
        let afterAction = protoGame
                            |> GameState.updatePlayer aId (fun player -> {player with hand = (Action Library)::origHand; deck = deck; discard = []})
                            |> GameStateUpdate.act aId (ALibrary <| LibraryChoice (function Smithy -> Discard | _ -> NoDiscard))
                            |> GameState.getPlayer aId
        afterAction
        |> GameState.getHand
        |> List.length
        |> should equal LIBRARY_CARD_COUNT
        afterAction 
        |> GameState.getHand
        |> memberEquals <| origHand @ (Utils.withoutFirst ((=) (Action Smithy)) deck |> List.rev |> List.tail |> List.rev)
        afterAction
        |> GameState.getDeck |> should equal [deck |> List.rev |> List.head]
        afterAction
        |> GameState.getDiscard |> memberEquals [Action Smithy; Action Library] 

    let [<Test>] ``library already has enough cards`` () =
        let aId = PId 0
        let origHand = List.replicate LIBRARY_CARD_COUNT (Coin Copper)
        protoGame
        |> GameState.updatePlayer aId (fun player -> {player with hand = [Action Library] @ origHand; deck = []; discard = []})
        |> GameStateUpdate.act aId (ALibrary <| LibraryChoice (fun _ -> Discard))
        |> GameState.getPlayer aId
        |> GameState.getHand
        |> memberEquals origHand

module BotTests =
    let buy toBuy hand game = 
        let id = PId 0
        (game 
        |> GameState.updatePlayer id (fun player -> {player with hand = hand})
        |> BotHandler.GameStateUpdate.applyFirstValidBuy id [(Always, toBuy)]
        |> GameState.getPlayer id).discard

    let [<Test>] ``pass bot does nothing`` () = 
        BotHandler.GameStateUpdate.applyFirstValidBuy (PId 0) [] protoGame 
        |> BotHandler.GameStateUpdate.applyFirstValidAction (PId 0) []
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
        let id = PId 0
        let deck = [Coin Copper; Victory Estate; Victory Duchy]
        (protoGame 
        |> GameState.updatePlayer id (fun player -> {player with hand = [Action Smithy]; deck = deck})
        |> BotHandler.GameStateUpdate.applyFirstValidAction id [(Always, ASmithy)]
        |> GameState.getPlayer id).hand 
        |> Set.ofList
        |> should equal (Set.ofList deck)

    let [<Test>] ``illegal action doesn't have card`` () =
        let id = PId 0
        let origHand = [Coin Copper]
        (protoGame 
        |> GameState.updatePlayer id (fun player -> {player with hand = origHand})
        |> BotHandler.GameStateUpdate.applyFirstValidAction id [(Always, ASmithy)]
        |> GameState.getPlayer id).hand 
        |> should equal origHand

    let [<Test>] ``illegal action not enough actions`` () =
        let id = PId 0
        let origHand = [Action Smithy]
        (protoGame 
        |> GameState.withTurn {protoGame.currentTurn with actions = 0}
        |> GameState.updatePlayer id (fun player -> {player with hand = origHand})
        |> BotHandler.GameStateUpdate.applyFirstValidAction id [(Always, ASmithy)]
        |> GameState.getPlayer id).hand 
        |> should equal origHand

    let [<Test>] ``evalCond always`` () = 
        BotHandler.evalCond protoGame (PId 0) Always
        |> should be True

    let [<Test>] ``evalCond count in deck true`` () =
        BotHandler.evalCond protoGame (PId 0) (CountInCardsLessThan (4, Action Smithy))
        |> should be True

    let [<Test>] ``evalCond count in deck false`` () =
        let count = 4
        let card = Action Smithy
        let cards = List.replicate count card
        let pId = PId 0
        let game = GameState.updatePlayer pId (fun p -> {p with deck = cards; hand = cards; discard = cards}) protoGame
        BotHandler.evalCond game pId (CountInCardsLessThan (count, card))
        |> should be False

    let [<Test>] ``evalCond expected per hand true`` () =
        BotHandler.evalCond protoGame (PId 0) (ExpectedPerHandLessThan (1.0, Action Smithy))
        |> should be True

    let [<Test>] ``evalCond expected per hand false`` () =
        let card = Action Smithy
        let pId = PId 0
        let game = GameState.updatePlayer pId (fun p -> {p with deck = List.replicate 100 card}) protoGame
        BotHandler.evalCond game pId (ExpectedPerHandLessThan (1., Action Smithy))
        |> should be False

    let [<Test>] ``evalCond cards remaining true`` () =
        BotHandler.evalCond {protoGame with cards = Map.empty} (PId 1) (CardsRemainingLessThan (1, Victory Province))
        |> should be True

    let [<Test>] ``evalCond cards remaining false`` () =
        BotHandler.evalCond protoGame (PId 1) (CardsRemainingLessThan (4, Victory Province))
        |> should be False

    let [<Test>] ``find valid action`` () =
        let aId = PId 0
        let toAct = ASmithy
        protoGame
        |> withActionCard aId Smithy
        |> GameStateUpdate.findFirstValidAction aId [(Always, AVillage);
                                                      (CountInCardsLessThan (100, Coin Gold), toAct);
                                                      (Always, ABureaucrat)]
        |> should equal <| Some toAct
        
    let [<Test>] ``find invalid action`` () =
        let aId = PId 0
        let toAct = ASmithy
        protoGame
        |> withActionCard aId Feast
        |> GameStateUpdate.findFirstValidAction aId [(Always, AVillage);
                                                      (Always, AFeast <| Victory Province);
                                                      (Always, ABureaucrat)]
        |> should equal None

    let [<Test>] ``find no valid action`` () =
        let aId = PId 0
        let toAct = ASmithy
        protoGame
        |> GameStateUpdate.findFirstValidAction aId [(Always, AVillage);
                                                      (CountInCardsLessThan (100, Coin Gold), toAct);
                                                      (Always, ABureaucrat)]
        |> should equal None

    let [<Test>] ``find valid buy`` () =
        let aId = PId 1
        let toBuy = Victory Duchy
        protoGame
        |> GameState.updatePlayer aId (fun player -> {player with hand = [Coin Silver; Coin Copper; Coin Copper; Coin Copper]}) 
        |> GameStateUpdate.findFirstValidBuy aId [(Always, Victory Province);
                                                  (Always, Coin Gold);
                                                  (ExpectedPerHandLessThan (2., Action Mine), toBuy);
                                                  (Always, Action Moat)]
        |> should equal <| Some toBuy

    let [<Test>] ``find no valid buy`` () =
        let aId = PId 1
        let toBuy = Action Mine
        protoGame
        |> GameState.updatePlayer aId (fun player -> {player with hand = [Coin Copper]}) 
        |> GameStateUpdate.findFirstValidBuy aId [(Always, Victory Province);
                                                  (Always, Coin Gold);
                                                  (ExpectedPerHandLessThan (2., Action Mine), toBuy);
                                                  (Always, Action Moat)]
        |> should equal None

    let [<Test>] ``action cards required`` () =
        let cards = [Smithy; Moat; Mine; Adventurer; Spy; Spy; Moat] 
        BotHandler.actionCardsRequired ["Marmalo", [], List.map (fun card -> (Always, Action card)) cards]
        |> should equal (Set.ofList cards)

    module GameStateUpdateTests =
        module BuyTests = 
            let preGameState id = protoGame |> GameState.updatePlayer id (fun player -> {player with hand = [Coin Gold]})
            let doBuy id toBuy = BotHandler.GameStateUpdate.buy id toBuy (preGameState id)

            let [<Test>] ``buy updates player discard`` () = let id = PId 0
                                                             let toBuy = Victory Estate
                                                             let afterBuy = doBuy id toBuy
                                                             (GameState.getPlayer id afterBuy).discard |> List.head |> should equal toBuy

            let [<Test>] ``buy updates card counts`` () = let id = PId 0
                                                          let toBuy = Victory Estate
                                                          let afterBuy = doBuy id toBuy
                                                          afterBuy.cards |> Map.find toBuy |> should equal
                                                             ((protoGame.cards |> Map.find toBuy) - 1)

            let [<Test>] ``buy lowers purchasing power`` () = let id = PId 0
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
    let [<Test>] trashFromDeck () = 
        let pId = PId 0
        let toTrash = Coin Copper
        protoGame
        |> GameState.updatePlayer pId (fun player -> {player with hand = []; deck = [toTrash]; discard = []})
        |> GameState.trashFromDeck toTrash pId 
        |> GameState.getPlayer pId
        |> Utils.allCards
        |> should equal []

    let [<Test>] ``can't draw more cards than the starting amount`` () =
        let card = Action Smithy
        let pId = PId 0
        let playerCount = List.length protoGame.players
        protoGame
        |> GameState.withCards [card]
        |> GameState.addCards ((initialCount playerCount card) + 1) pId card
        |> GameState.getPlayer pId
        |> GameState.getDiscard
        |> should equal <| List.replicate (initialCount playerCount card) card

    let [<Test>] discard () = let id = PId 0
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

module ExcelRendererTests =
    open ExcelRenderer

    (* Bot names are sorted alphabetically *)
    let placements = Map.ofList [("Foo", Map.ofList [(0, 2); (1, 1)]); ("Bar", Map.ofList [(0, 1); (1, 2)])]

    let [<Test>] botNames () =
        botNameLabels placements
        |> should equal <| Map.ofList [((Row 1, Col 0), "Foo"); ((Row 0, Col 0), "Bar")]

    let [<Test>] placeLabels () =
        placeLabels placements
        |> should equal <| Map.ofList [((Row 0, Col 0), 1); ((Row 0, Col 1), 2)]

    let [<Test>] testPlaceFreqs () =
        let actual = placeFreqs placements
        let expected = Map.ofList [((Row 0, Col 0), 1); ((Row 0, Col 1), 2); ((Row 1, Col 0), 2); ((Row 1, Col 1), 1)]
        actual |> should equal expected

    let [<Test>] placeFreqs3 () =
        let actual = placeFreqs <| Map.ofList ["MineSmithy", Map.ofList [0, 3]; "Smithy", Map.ofList [1, 3]; "Pass", Map.ofList [2, 3]]
        let expected = Map.ofList [(Row 0, Col 0), 3; (Row 0, Col 1), 0; (Row 0, Col 2), 0;
                                   (Row 1, Col 0), 0; (Row 1, Col 1), 3; (Row 1, Col 2), 0;
                                   (Row 2, Col 0), 0; (Row 2, Col 1), 0; (Row 2, Col 2), 3]
        actual |> should equal expected

module GameTests =
    let [<Test>] ``get initial bots`` () =
        let bot = "Foo", [], []
        (Dominion.Game.getInitialState [bot]).players |> List.length |> should equal 1

    let [<Test>] ``playGame doesn't crash`` () = Dominion.Game.playGame () |> Dominion.Game.gameOver |> should be True

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
        let [<Test>] ``game not over initially`` () = Dominion.Game.gameOver protoGame |> should be False
    
        let [<Test>] ``game over after turn limit`` () = Dominion.Game.gameOver
                                                            {GameState.initialGameState with turnsTaken = Constants.TURN_LIMIT + 1}
                                                            |> should be True

        let [<Test>] ``game over when provinces gone`` () = Dominion.Game.gameOver
                                                                {GameState.initialGameState with
                                                                    cards = Map.add (Victory Province) 0 GameState.initialGameState.cards}
                                                            |> should be True

        let [<Test>] ``game over when three cards gone`` () = let cards = protoGame.cards 
                                                                            |> Map.add (Victory Estate) 0
                                                                            |> Map.add (Coin Copper) 0
                                                                            |> Map.add (Action Smithy) 0
                                                              Dominion.Game.gameOver {GameState.initialGameState with cards = cards}
                                                            |> should be True

        let [<Test>] ``game not over when two cards gone`` () = let cards = protoGame.cards 
                                                                            |> Map.add (Victory Estate) 0
                                                                            |> Map.add (Action Smithy) 0
                                                                Dominion.Game.gameOver {GameState.initialGameState with cards = cards}
                                                                |> should be False
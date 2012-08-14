module ActionCards

open Definitions
open Constants

type action = int -> gameState -> gameState

(* TODO these fail to take Moat into account *)

(* Maybe validation should be separated out from the actual action logic.
   One advantage of not separating it out is that it's easier to be more generous with fall-back behavior, 
   instead of just flatly rejecting ill-formed actions. *)
(* there are many edge cases that this probably doesn't have correct behavior for *)
let rec actionOfCard = function
  | ASmithy -> fun id gameState -> GameState.updatePlayer id (GameState.draw SMITHY_CARDS_DRAW) gameState
  | ACellar toDiscard -> fun id gameState -> List.fold (fun gameState card -> gameState
                                                                                |> GameState.discard card id
                                                                                |> GameState.drawFor 1 id)
                                                gameState toDiscard
                                           |> GameState.addActions 1

  | AChapel (card1, card2, card3, card4) -> fun id gameState -> List.fold
                                                                    (fun gameState card -> GameState.trash card id gameState) gameState
                                                                    (Utils.withoutNone [card1; card2; card3; card4])
  
  | AChancellor reshuffle -> fun id gameState -> 
                                (match reshuffle with
                                    | Reshuffle -> GameState.updatePlayer id
                                                    (fun player -> {player with deck = []; discard = player.discard @ player.deck}) gameState
                                    | NoReshuffle -> gameState) |> GameState.addPurchasingPower CHANCELLOR_PURCHASING_POWER
  
  | AVillage -> fun id gameState -> gameState
                                        |> GameState.addActions 1
                                        |> GameState.drawFor 1 id
  
  | AWoodcutter -> fun id gameState -> gameState
                                        |> GameState.addPurchasingPower WOODCUTTER_PURCHASING_POWER
                                        |> GameState.addBuys WOODCUTTER_BUYS 
  
  | AFeast toGain as feast -> if cardCost toGain > 5
                                then invalidArg "toGain"
                                         (sprintf "Feast can only give a card costing up to 5, but %A costs %d" toGain (cardCost toGain))
                                else fun pId gameState -> gameState
                                                            |> GameState.trash (Action Feast) pId
                                                            |> GameState.gainCard toGain pId

  | AMilitia -> fun aId gameState -> GameState.getIdRange gameState
                                    |> Seq.filter (fun pId -> pId <> aId && not <| GameState.hasMoat pId gameState)
                                    |> Seq.fold (fun game pId -> GameState.updatePlayer pId
                                                                    (fun player -> let card1, card2, card3 = player.militiaReaction player.hand
                                                                                   let drawnDownHand = Utils.withoutNone [card1; card2; card3]
                                                                                                       |> Utils.ensureSubset player.hand
                                                                                                       |> Utils.fillHand player.hand
                                                                                   {player with hand = drawnDownHand}) game)
                                                gameState
  
  | AMoneylender -> fun id gameState -> if not (Utils.listMem (GameState.getPlayer id gameState).hand (Coin Copper))
                                           then gameState
                                           else gameState
                                                |> GameState.updatePlayer id
                                                    (fun player -> {player with hand = Utils.withoutFirst ((=) (Coin Copper)) player.hand})
                                                |> GameState.addPurchasingPower MONEYLENDER_PURCHASING_POWER
  
  | ARemodel (toRemodel, toGain) ->
        fun id gameState -> 
            let cardCostOk = cardCost toRemodel + 2 >= cardCost toGain                     
            let hasToRemodel = Utils.listMem (GameState.getPlayer id gameState).hand toRemodel
            if not (cardCostOk && hasToRemodel)
                then gameState
                else GameState.trash toRemodel id gameState
                        |> GameState.gainCard toGain id
                
  | AThroneRoom act -> fun id gameState -> if (GameState.getPlayer id gameState).hand
                                                |> Utils.contains (Action <| Definitions.getRaw act) |> not
                                              then gameState
                                              else (* TODO if act itself is a ThroneRoom, you're not allowed to play one card 4 times. *)
                                                  let action = (actionOfCard act) id
                                                  gameState |> action |> action
                                                  |> GameState.discard (Action <| Definitions.getRaw act) id

  | ACouncilRoom -> fun aId gameState -> gameState
                                        |> GameState.addBuys COUNCIL_ROOM_BUYS
                                        |> GameState.drawFor COUNCIL_ROOM_SELF_DRAW_COUNT aId
                                        |> Seq.fold (fun game pId -> GameState.drawFor COUNCIL_ROOM_OTHER_DRAW_COUNT pId game)
                                        <| (GameState.getIdRange gameState |> Seq.filter ((<>) aId))

  | AFestival -> fun aId gameState -> gameState
                                        |> GameState.addActions FESTIVAL_ACTIONS
                                        |> GameState.addBuys FESTIVAL_BUYS
                                        |> GameState.addPurchasingPower FESTIVAL_PURCHASE_POWER

  | ALaboratory -> fun aId gameState -> gameState
                                        |> GameState.addActions LAB_ACTIONS
                                        |> GameState.drawFor LAB_DRAW_COUNT aId

  | AMarket -> fun aId gameState -> gameState
                                    |> GameState.addActions MARKET_ACTIONS
                                    |> GameState.addBuys MARKET_BUYS
                                    |> GameState.addPurchasingPower MARKET_PURCHASING_POWER
                                    |> GameState.drawFor MARKET_CARDS aId

  | AMine toMine -> fun aId gameState -> if Utils.listMem (GameState.getPlayer aId gameState).hand (Coin toMine) |> not
                                            then gameState
                                            else
                                                (* Technically, you can trash a treasure for an equivalent or shittier one, 
                                                   but that seems extraordinarily unlikely to be a good idea from the player's
                                                   perspective, so I'm just going to not implement it for now. *)
                                                let toGain = match toMine with
                                                             | Copper -> Silver
                                                             | _ -> Gold 
                                                gameState 
                                                    |> GameState.trash (Coin toMine) aId
                                                    |> GameState.updatePlayer aId (fun player -> {player with hand = (Coin toGain)::player.hand})
                                    
  | AWitch -> fun aId gameState -> Seq.fold (fun game pId -> match pId with
                                                              | _id when _id = aId -> GameState.drawFor WITCH_DRAW_COUNT aId game
                                                              | _id when GameState.hasMoat _id game -> game
                                                              | _ -> GameState.addCards WITCH_CURSE_COUNT pId (Victory Curse) game)
                                            gameState
                                            <| GameState.getIdRange gameState

  | AAdventurer -> fun aId -> let rec helper soFar notTreasure player =
                                         let finalPlayer = {player with discard = player.discard @ notTreasure; hand = player.hand @ soFar}
                                         if List.length soFar >= ADVENTURER_TREASURE_COUNT
                                         then finalPlayer
                                         else
                                            match player.deck with
                                            |  ((Coin c) as treasure)::tl -> helper (treasure::soFar) notTreasure {player with deck = tl} 
                                            |  hd::tl -> helper soFar (hd::notTreasure) {player with deck = tl} 
                                            |  [] -> match player.discard with
                                                     | [] -> finalPlayer
                                                     | _ -> helper soFar notTreasure {player with deck = Utils.shuffle player.discard; discard = []}
                              GameState.updatePlayer aId (helper [] [])

  | ASpy (SpyChoice (chooseSelfCard, chooseOtherCard)) ->
        fun aId gameState ->
            gameState
            |> GameState.drawFor SPY_CARD_COUNT aId
            |> GameState.foldPlayers
                (fun pId player -> if pId <> aId
                                        && GameState.hasMoat pId gameState
                                    then player
                                    else
                                        let choice = if pId = aId then chooseSelfCard else chooseOtherCard
                                        match player.deck with
                                        | hd::tl -> match choice hd with
                                                    | Discard -> {player with deck = tl; discard = hd::player.discard}
                                                    | NoDiscard -> player
                                        (* TODO shouldn't this look into the discard if the deck is empty? *)
                                        | [] -> player)

  | AThief (ThiefChoice (priorities, shouldGain)) ->
    fun aId gameState -> 
        List.fold (fun game pId -> if pId = aId || GameState.hasMoat pId game
                                   then game
                                   else 
                                        let withCardCount = GameState.ensureCardCountInDeck pId THIEF_CARD_COUNT game
                                        let revealedCards = GameState.getPlayer pId withCardCount
                                                            |> GameState.getDeck
                                                            |> List.toSeq
                                                            |> Seq.truncate THIEF_CARD_COUNT
                                                            |> Seq.toList
                                        let revealedCoins = List.filter (function Coin coin -> true | _ -> false) revealedCards
                                        let toTrash = match revealedCoins with
                                                        | [] -> None
                                                        | coins -> coins
                                                                    |> Seq.map (function Coin coin -> coin | _ -> failwith "expecting coin")
                                                                    |> Seq.sortBy (fun coin -> priorities coin)
                                                                    |> Seq.head
                                                                    |> Some
                                        let toDiscard = match toTrash with
                                                        | None -> revealedCards
                                                        | Some coin -> Utils.withoutFirst ((=) (Coin coin)) revealedCards
                                        let afterDiscard = withCardCount
                                                            |> GameState.discardCardsFromDeck toDiscard pId 
                                        match toTrash with
                                        |   None -> afterDiscard
                                        |   Some coin -> afterDiscard
                                                         |> GameState.trashFromDeck (Coin coin) pId 
                                                         |> match shouldGain coin with
                                                            (* Technically the card isn't being 'removed' from the trash,
                                                               but I don't think that actually affects anything. *)
                                                            | Gain -> GameState.addCards 1 aId (Coin coin) 
                                                            | NoGain -> id
                                        )
                gameState <| (Seq.toList <| GameState.getIdRange gameState)
                                        
  | ALibrary (LibraryChoice shouldDiscard) ->
        fun aId gameState ->
            let rec helper game =
                    (* LIBRARY_CARD_COUNT + 1 because the Library is still in the hand at this point *)
                if GameState.getPlayer aId game |> GameState.getHand |> List.length = LIBRARY_CARD_COUNT + 1
                    || (GameState.deckLen aId game = 0 && GameState.getPlayer aId game |> GameState.getDiscard |> List.length = 0)
                (* "If you run out of cards even after shuffling, you just get however many there were." *)
                then game
                else GameState.updatePlayer
                        aId 
                        (fun player -> match player.deck with
                                        | ((Action action) as actCard)::tl -> match shouldDiscard action with
                                                                              | Discard -> {player with discard=actCard::player.discard; deck=tl}
                                                                              | NoDiscard -> {player with hand=actCard::player.hand; deck=tl}   
                                        | hd::tl -> {player with hand=hd::player.hand; deck=tl}
                                        | [] -> GameState.refillDeck player)
                        game
                        |> helper
            helper gameState

    | unrecognized -> failwith <| sprintf "unrecognized action card: %A" unrecognized
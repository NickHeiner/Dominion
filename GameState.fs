module GameState

open Definitions
open Constants

(* This would be defined in Constants if not for mutual recursion issues *)
let initialTurn = {actions = 1; buys = 1; purchasingPower = 0}  

(* This needs to be a function and not just a value so `shuffle` is called multiple times. *)
let initialPlayer () = {hand = []
                        discard = []
                        deck = List.replicate 7 (Coin Copper) @ List.replicate 3 (Victory Estate) |> Utils.shuffle
                        bot = "Pass", [], []
                        militiaReaction = fun _ -> (None, None, None)}

let initialGameState = {players = []; cards = Map.empty; trash = []; currentTurn = initialTurn; roundsPlayed = 0; log = [] }

(* Resets the card counts to the initial amount of each card in `cards`. *)
let withCards cards game = 
    let playerCount = List.length game.players
    {game with cards = List.fold (fun acc el -> Map.add el (initialCount playerCount el) acc) Map.empty cards}

let nextTurn gameState = {gameState with currentTurn = initialTurn}

let getPlayer (PId index) gameState = List.nth gameState.players index
                            
let updatePlayer ((PId index) as pId) update gameState =
  {gameState with players = getPlayer pId gameState
                            |> update
                            |> Utils.withNth gameState.players index}

let withTurn turn gameState = {gameState with currentTurn = turn}
let addActions count gameState = withTurn {gameState.currentTurn with actions = gameState.currentTurn.actions + count} gameState
let addBuys count gameState = withTurn {gameState.currentTurn with buys = gameState.currentTurn.buys + count} gameState
let addPurchasingPower amount gameState = withTurn {gameState.currentTurn with purchasingPower = gameState.currentTurn.purchasingPower + amount} gameState

let getHand player = player.hand
let setHand player hand = {player with hand = hand}

let getDeck player = player.deck
let setDeck player deck = {player with deck = deck}

let getDiscard player = player.discard

let getPlayers game = game.players
let getLog game = game.log

let totalPurchasingPower pId gameState = 
  gameState.currentTurn.purchasingPower + List.sumBy purchasingPowerOf (getPlayer pId gameState |> getHand)

let refillDeck player = {player with deck = Utils.shuffle player.discard; discard = []} 

let rec draw count player =
  if count < 0 then invalidArg "count" <| sprintf "can't draw negative cards, but count was: %d" count else
  if count = 0 then player else 
    match player.deck with 
      | hd::tl -> draw (count - 1) {player with hand = hd::player.hand; deck = tl}
      | [] -> draw count <| refillDeck player

let drawFor count pId = updatePlayer pId (fun player -> draw count player)

let discardAll player = {player with discard = player.hand @ player.discard; hand = [] }

type requireCardInHand = Yes | No

let removeCard getDiscard setCardSource cardSource requireCard card id gameState =
    match List.tryFind ((=) card) (cardSource (getPlayer id gameState)) with
        | Some _ -> updatePlayer id
                        (fun player -> {setCardSource player (Utils.withoutFirst ((=) card) (cardSource player))
                                            with discard = getDiscard card player.discard})
                        gameState
        | None -> match requireCard with
                    | Yes -> invalidArg "card" (sprintf "Player %A does not have card %A" id card)
                    | No -> gameState

(* Ensures that `card` is not in the player's hand *)
let safeDiscard card = removeCard (fun card discard -> card::discard) setHand getHand No card

let discard card = removeCard (fun card discard -> card::discard) setHand getHand Yes card
let discardFromDeck card = removeCard (fun card discard -> card::discard) setDeck getDeck Yes card
let discardCardsFromDeck cards pId initGame = List.fold (fun game card -> discardFromDeck card pId game) initGame cards
let trash = removeCard (fun _ discard -> discard) setHand getHand Yes
let trashFromDeck = removeCard (fun _ discard -> discard) (fun player deck -> {player with deck = deck}) (fun player -> player.deck) Yes

let getIdRange gameState = Seq.map PId { 0 .. List.length gameState.players - 1} 

let _addCardsForPlayer transformPlayer count pId card game = 
    if not <| Map.containsKey card game.cards
    then
        invalidArg "card" <| sprintf "card %A is not in the game. Is it missing from the allCards set?" card
    else
        let priorCardCount = Map.find card game.cards
        let amountToAdd = min count priorCardCount
        {game with cards = Map.add card (priorCardCount - amountToAdd) game.cards}
        |> updatePlayer pId (transformPlayer amountToAdd card)

let addCards = _addCardsForPlayer (fun amountToAdd card player -> {player with discard = (List.replicate amountToAdd card)@player.discard})
let addCardToDeck = _addCardsForPlayer (fun amountToAdd card player -> {player with deck = (List.replicate amountToAdd card)@player.deck}) 1
let gainCard = addCards 1

let foldByPlayers f gameState = Seq.fold f gameState <| getIdRange gameState
let foldPlayers f gameState = Seq.fold (fun game pId -> updatePlayer pId (f pId) game) gameState <| getIdRange gameState

let getActs pId gameState = (getPlayer pId gameState).bot |> Utils.mid
let getBuys pId gameState = (getPlayer pId gameState).bot |> Utils.thd

let deckLen pId gameState = getPlayer pId gameState
                            |> getDeck
                            |> List.length

(* Shuffles the entire discard into the deck if the deck currently has fewer than `count` cards *)
let ensureCardCountInDeck pId count gameState = if deckLen pId gameState >= count
                                                then gameState
                                                else updatePlayer pId refillDeck gameState

let hasMoat pId game = getPlayer pId game
                        |> getHand
                        |> Utils.contains (Action Moat)

let cardCount aId card =
    getPlayer aId
    >> Utils.allCards
    >> Utils.countOccurs card

module Definitions

type VictCard = Province | Duchy | Estate | Gardens | Curse
type CoinCard = Gold | Silver | Copper 

type ActCard = Cellar | Chapel  | Chancellor | Village | Woodcutter | Feast | Militia | Moneylender | Remodel
                | Smithy | Spy | Thief | ThroneRoom | CouncilRoom | Festival | Laboratory | Library | Market
                | Mine | Witch | Adventurer

type card = Victory of VictCard | Coin of CoinCard | Action of ActCard

type reshuffle = Reshuffle | NoReshuffle
type discard = Discard | NoDiscard

type pId = PId of int

(* Is "keep" really an option? *)
type thiefReaction = Gain of CoinCard | Trash of CoinCard | Keep of CoinCard

[<CustomEquality; CustomComparison>]
type spyChoice = 
  SpyChoice of (card -> discard) * (card -> discard)
      override x.Equals(y) = (match y with :? spyChoice -> true | _ -> false)
      override x.GetHashCode() = 0
      interface System.IComparable with
        member x.CompareTo(y) = (match y with :? spyChoice -> 0 | _ -> failwith "wrong type")

[<CustomEquality; CustomComparison>]
type thiefChoice = 
    ThiefChoice of (CoinCard option -> CoinCard option -> thiefReaction)
        override x.Equals(y) = (match y with :? thiefChoice -> true | _ -> false)
        override x.GetHashCode() = 0
        interface System.IComparable with
            member x.CompareTo(y) = (match y with :? thiefChoice -> 0 | _ -> failwith "wrong type")

type argActCard = ACellar of card list | AChapel of card option * card option* card option * card option
                | AChancellor of reshuffle | AVillage | AWoodcutter | AFeast of card | AMilitia | AMoneylender | ARemodel of card * card
                | ASmithy | ASpy of spyChoice | AThief of thiefChoice | AThroneRoom of argActCard 
                | ACouncilRoom | AFestival | ALaboratory | ALibrary | AMarket
                | AMine of CoinCard | AWitch | AAdventurer

let getRaw = function
                | ACellar _ -> Cellar
                | AChapel _ -> Chapel
                | AChancellor _ -> Chancellor
                | AVillage -> Village
                | AWoodcutter -> Woodcutter
                | AFeast _ -> Feast
                | AMilitia -> Militia
                | AMoneylender -> Moneylender
                | ARemodel _ -> Remodel
                | ASmithy -> Smithy
                | ASpy _ -> Spy
                | AThief _ -> Thief
                | AThroneRoom _ -> ThroneRoom
                | ACouncilRoom -> CouncilRoom
                | AFestival -> Festival
                | ALaboratory -> Laboratory
                | ALibrary -> Library
                | AMarket -> Market
                | AMine _ -> Mine
                | AWitch -> Witch
                | AAdventurer -> Adventurer

(* Turn for a player. purchasingPower doesn't include coins. *)
type turn = {actions : int; buys : int; purchasingPower : int}

type act = Act of ActCard
type buy = Buy of card

type bot = argActCard list * buy list

[<CustomEquality; NoComparisonAttribute>]
type player = {hand : card list; deck : card list; discard : card list; bot : bot;
                militiaReaction : card list -> (card option * card option * card option)}
    
                override x.Equals(yobj) = 
                    match yobj with
                    | :? player as y -> x.hand = y.hand && x.deck = y.deck && x.discard = y.discard && x.bot = y.bot
                    | _ -> false             
                    
                override x.GetHashCode() = hash (x.hand, x.deck, x.discard, x.bot)

type gameState = {players : player list; cards : Map<card, int>; trash : card list; currentTurn : turn; turnsTaken : int}

(* this there a better way to enumerate over all members of the type? *)
(* http://stackoverflow.com/questions/10867544/map-over-all-values-in-a-discriminated-union *)
let allCards = [Victory Province; Victory Duchy; Victory Estate; Coin Gold; Coin Silver; Coin Copper; Action Smithy]

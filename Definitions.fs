module Definitions

type VictCard = Province | Duchy | Estate | Gardens | Curse
type CoinCard = Gold | Silver | Copper 

type ActCard = Cellar | Chapel  | Chancellor | Village | Woodcutter | Feast | Militia | Moneylender | Remodel
                | Smithy | Spy | Thief | ThroneRoom | CouncilRoom | Festival | Laboratory | Library | Market
                | Mine | Witch | Workshop | Moat | Bureaucrat | Adventurer

type card = Victory of VictCard | Coin of CoinCard | Action of ActCard

type reshuffle = Reshuffle | NoReshuffle
type discard = Discard | NoDiscard

type pId = PId of int

[<CustomEquality; CustomComparison>]
type spyChoice = 
  SpyChoice of (card -> discard) * (card -> discard)
      override x.Equals(y) = (match y with :? spyChoice -> true | _ -> false)
      override x.GetHashCode() = 0
      interface System.IComparable with
        member x.CompareTo(y) = (match y with :? spyChoice -> 0 | _ -> failwith "wrong type")

(* priority must be written in this order, or automatic comparison won't work as expected *)
type priority = First | Second | Third
type gain = Gain | NoGain

[<CustomEquality; CustomComparison>]
type thiefChoice = 
    ThiefChoice of (CoinCard -> priority) * (CoinCard -> gain)
        override x.Equals(y) = (match y with :? thiefChoice -> true | _ -> false)
        override x.GetHashCode() = 0
        interface System.IComparable with
            member x.CompareTo(y) = (match y with :? thiefChoice -> 0 | _ -> failwith "wrong type")

[<CustomEquality; CustomComparison>]
type cellarChoice =
    CellarChoice of (card list -> card list)
        override x.Equals(y) = (match y with :? cellarChoice -> true | _ -> false)
        override x.GetHashCode() = 0
        interface System.IComparable with
            member x.CompareTo(y) = (match y with :? cellarChoice -> 0 | _ -> failwith "wrong type")

[<CustomEquality; CustomComparison>]
type libraryChoice =
    LibraryChoice of (ActCard -> discard)
        override x.Equals(y) = (match y with :? libraryChoice -> true | _ -> false)
        override x.GetHashCode() = 0
        interface System.IComparable with
            member x.CompareTo(y) = (match y with :? libraryChoice -> 0 | _ -> failwith "wrong type")

type argActCard = ACellar of cellarChoice | AChapel of card option * card option* card option * card option
                | AChancellor of reshuffle | AVillage | AWoodcutter | AFeast of card | AMilitia | AMoneylender | ARemodel of card * card
                | ASmithy | ASpy of spyChoice | AThief of thiefChoice | AThroneRoom of argActCard 
                | ACouncilRoom | AFestival | ALaboratory | ALibrary of libraryChoice | AMarket
                | AMine of CoinCard | AWitch | AAdventurer | AMoat | ABureaucrat | AWorkshop of card

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
                | ALibrary _ -> Library
                | AMarket -> Market
                | AMine _ -> Mine
                | AWitch -> Witch
                | AAdventurer -> Adventurer
                | AMoat -> Moat
                | ABureaucrat -> Bureaucrat
                | AWorkshop _ -> Workshop

(* Turn for a player. purchasingPower doesn't include coins. *)
type turn = {actions : int; buys : int; purchasingPower : int}

type cond = Always | ExpectedPerHandLessThan of float * card | CountInCardsLessThan of int * card | CardsRemainingLessThan of int * card
type act = (cond * argActCard) list
type buy = (cond * card) list
type bot = string * act * buy (* string = name *)

[<CustomEquality; NoComparisonAttribute>]
(* Why are hand and discard lists and not sets? *)
type player = {hand : card list; deck : card list; discard : card list; bot : bot;
                militiaReaction : card list -> (card option * card option * card option)}
    
                override x.Equals(yobj) = 
                    match yobj with
                    | :? player as y -> x.hand = y.hand && x.deck = y.deck && x.discard = y.discard && x.bot = y.bot
                    | _ -> false             
                    
                override x.GetHashCode() = hash (x.hand, x.deck, x.discard, x.bot)

(* each round = every player taking a turn *)
type gameState = {players : player list; cards : Map<card, int>; trash : card list; currentTurn : turn; roundsPlayed : int}
(* TODO do score and cardCounts need to be floats? If we're not using these types for aggregation, I don't think so. *)
type playerStats = {name: string; score: float; cardCounts : Map<card, float>}
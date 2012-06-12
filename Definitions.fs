module Definitions

type reshuffle = Reshuffle | NoReshuffle

type VictCard = Province | Duchy | Estate | Gardens | Curse
type CoinCard = Gold | Silver | Copper 

(* [<CustomEquality; NoComparisonAttribute>] *)
type ActCard = Cellar of card list | Chapel of (card option * card option* card option* card option) 
                | Chancellor of reshuffle | Village | Woodcutter | Feast of card | Militia | Moneylender | Remodel of (card * card) 
                | Smithy | Spy (* of (card -> bool * card -> bool) *) | Thief | ThroneRoom of ActCard 
                | CouncilRoom | Festival | Laboratory | Library | Market
                | Mine | Witch | Adventurer
                (*
                override x.Equals(yobj) =
                    match yobj with
                    *)
and card = Victory of VictCard | Coin of CoinCard | Action of ActCard

(* Turn for a player. purchasingPower doesn't include coins. *)
type turn = {actions : int; buys : int; purchasingPower : int}

type act = Act of ActCard
type buy = Buy of card

type bot = act list * buy list

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

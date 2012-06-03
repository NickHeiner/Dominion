module Definitions

type VictCard = Province | Duchy | Estate | Gardens | Curse
type CoinCard = Gold | Silver | Copper 
type ActCard = Cellar | Chapel | Chancellor | Village | Woodcutter | Feast | Militia | Moneylender | Remodel | Smithy 
                | Spy | Thief | ThroneRoom | CouncilRoom | Festival | Laboratory | Library | Market | Mine | Witch | Adventurer

type card = Victory of VictCard | Coin of CoinCard | Action of ActCard

(* Turn for a player. purchasingPower doesn't include coins. *)
type turn = {actions : int; buys : int; purchasingPower : int}

type gameState = {players : player list; cards : Map<card, int>; trash : card list; currentTurn : turn; turnsTaken : int}
and bot = int -> gameState -> gameState 
and player = {hand : card list; deck : card list; discard : card list; bot : bot}

(* this there a better way to enumerate over all members of the type? *)
(* http://stackoverflow.com/questions/10867544/map-over-all-values-in-a-discriminated-union *)
let allCards = [Victory Province; Victory Duchy; Victory Estate; Coin Copper; Action Smithy]

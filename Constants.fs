module Constants

open Definitions

let TURN_LIMIT = 10000

(* How many cards are required for a single garden to be worth a single point *)
let GARDENS_FACTOR = 10

(* Starting number of cards in a hand *)
let CARDS_PER_HAND = 5

let SMITHY_CARDS_DRAW = 3
let CHANCELLOR_PURCHASING_POWER = 2
let WOODCUTTER_PURCHASING_POWER = 2
let WOODCUTTER_BUYS = 1
let MILITIA_DRAW_DOWN_COUNT = 3
let MONEYLENDER_PURCHASING_POWER = 3
let COUNCIL_ROOM_SELF_DRAW_COUNT = 4
let COUNCIL_ROOM_OTHER_DRAW_COUNT = 1
let COUNCIL_ROOM_BUYS = 1
let FESTIVAL_ACTIONS = 2
let FESTIVAL_PURCHASE_POWER = 2
let FESTIVAL_BUYS = 1
let LAB_DRAW_COUNT = 2
let LAB_ACTIONS = 1
let MARKET_ACTIONS = 1
let MARKET_CARDS = 1
let MARKET_BUYS = 1
let MARKET_PURCHASING_POWER = 1
let WITCH_DRAW_COUNT = 2
let WITCH_CURSE_COUNT = 1
let ADVENTURER_TREASURE_COUNT = 2
let SPY_CARD_COUNT = 1

let initialPlayer = {hand=[]; discard=[]; deck = List.replicate 7 (Coin Copper) @ List.replicate 3 (Victory Estate); bot = [], []; militiaReaction = fun _ -> (None, None, None)}

let cardCost = function
  | Victory v -> match v with 
                  | Province -> 8
                  | Duchy -> 5
                  | Estate -> 2
                  | Gardens -> 4
                  | Curse -> 0
  | Coin c -> match c with
                  | Gold -> 6
                  | Silver -> 3
                  | Copper -> 0
  | Action a -> match a with
                  | Cellar _ -> 2
                  | Chapel _ -> 2
                  | Chancellor _ -> 3 
                  | Village -> 3
                  | Woodcutter -> 3
                  | Feast _ -> 4
                  | Militia -> 4
                  | Moneylender -> 4
                  | Remodel _ -> 4
                  | Smithy -> 4 
                  | Spy _ -> 4
                  | Thief -> 4
                  | ThroneRoom _ -> 4
                  | CouncilRoom -> 5
                  | Festival -> 5
                  | Laboratory -> 5
                  | Library -> 5
                  | Market -> 5
                  | Mine _ -> 5
                  | Witch -> 5
                  | Adventurer -> 6

let purchasingPowerOf = function
  | Coin c -> match c with
                | Gold -> 3
                | Silver -> 2
                | Copper -> 1
  | _ -> 0

let victoryPointsFor = function
  | Victory v -> match v with
                   | Province -> 6
                   | Duchy -> 3
                   | Estate -> 1
                   | Curse -> -1
                   | Gardens -> 0
  | _ -> 0

(* Note: only a set of actions should have initial count 10; the rest have initial count 0 *)
let initialCount = function
  | Victory v -> match v with
                  | Province -> 12 
                  | Duchy -> 12 
                  | Estate -> 24 
                  | Gardens -> 12
                  | Curse -> 30 
  | Coin c -> match c with 
                  | Gold -> 30 
                  | Silver -> 40 
                  | Copper -> 60
  | Action _ -> 10
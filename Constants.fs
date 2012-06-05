module Constants

open Definitions

let TURN_LIMIT = 10000

(* How many cards are required for a single garden to be worth a single point *)
let GARDENS_FACTOR = 10

(* Starting number of cards in a hand *)
let CARDS_PER_HAND = 5

let SMITHY_CARDS_DRAW = 3

let initialPlayer = {hand=[]; discard=[]; deck= List.replicate 7 (Coin Copper) @ List.replicate 7 (Victory Estate); bot = []}

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
                  | Cellar -> 2
                  | Chapel -> 2
                  | Chancellor -> 3 
                  | Village -> 3
                  | Woodcutter -> 3
                  | Feast -> 4
                  | Militia -> 4
                  | Moneylender -> 4
                  | Remodel -> 4
                  | Smithy -> 4 
                  | Spy -> 4
                  | Thief -> 4
                  | ThroneRoom -> 4
                  | CouncilRoom -> 5
                  | Festival -> 5
                  | Laboratory -> 5
                  | Library -> 5
                  | Market -> 5
                  | Mine -> 5
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
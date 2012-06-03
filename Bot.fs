module Bot

open Definitions

let canBuy id gameState card = GameState.totalPurchasingPower id gameState > Constants.cardCost card
let buyIfPossible id card gameState = if canBuy id gameState card then GameState.buy id card gameState else gameState
let drawCardOdds player card = let allCards = Utils.allCards player
                               float (Utils.countOccurences allCards card) / float (List.length allCards)
let expectedPerHand player card = drawCardOdds player card * float Constants.CARDS_PER_HAND
let buyAll id cards gameState = 
  List.fold (fun gameState card -> buyIfPossible id card gameState) gameState [Victory Province; Coin Gold; Coin Silver; Coin Copper]

let estateBot id gameState = buyIfPossible id (Victory Estate) gameState 

let cashBot id gameState =
  buyAll id [Victory Province; Coin Gold; Coin Silver; Coin Copper] gameState 
  
let smithyBot id gameState = 
  buyIfPossible id (Victory Province) gameState
  |> if expectedPerHand (List.nth gameState.players id) (Action Smithy) < 1.0
       then buyIfPossible id (Action Smithy)
       else buyAll id [Coin Gold; Coin Silver; Coin Copper]
  
let bots = [("Pass", (fun _ x -> x)); ("Estate", estateBot); ("Cash", cashBot); ("Smithy", smithyBot)]
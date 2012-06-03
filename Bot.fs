module Bot

open Definitions

let canBuy id gameState card = GameState.totalPurchasingPower id gameState > Constants.cardCost card
let buyIfPossible id card gameState = if canBuy id gameState card then GameState.buy id card gameState else gameState

let estateBot id gameState = buyIfPossible id (Victory Estate) gameState 

let cashBot id gameState =
  List.fold (fun gameState card -> buyIfPossible id card gameState) gameState [Victory Province; Coin Gold; Coin Silver; Coin Copper]

let bots = [("Estate", estateBot); ("Cash", cashBot); ("Pass", (fun _ x -> x))]
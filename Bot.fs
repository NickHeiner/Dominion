module Bot

open Definitions

let estateBot id gameState = 
  if GameState.totalPurchasingPower id gameState > Constants.cardCost (Victory Estate)
    then GameState.buy id (Victory Estate) gameState
    else gameState
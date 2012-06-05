module ActionCards

open Definitions
open Constants

type action = int -> gameState -> gameState

let actionOfCard = function
  | Smithy -> fun id gameState -> GameState.updatePlayer id (GameState.draw SMITHY_CARDS_DRAW) gameState
  | Cellar -> fun _ gameState -> GameState.addActions 1 gameState
  | _ -> failwith "not impl"

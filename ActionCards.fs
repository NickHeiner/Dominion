module ActionCards

open Definitions

type action = int -> GameState.t -> GameState.t

let actionOfCard = function
  | Smithy -> fun id gameState -> GameState.updatePlayer id (fun player -> GameState.draw player 3) gameState
  | Cellar -> fun _ gameState -> GameState.addActions 1 gameState
  | _ -> failwith "not impl"


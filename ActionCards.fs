module ActionCards

open Definitions

type action = int -> gameState -> gameState

let actionOfCard = function
  | Smithy -> fun id gameState -> GameState.updatePlayer id (GameState.draw 3) gameState
  | Cellar -> fun _ gameState -> GameState.addActions 1 gameState
  | _ -> failwith "not impl"

(* let playCard card gameState = gameState |> actionOfCard card *)


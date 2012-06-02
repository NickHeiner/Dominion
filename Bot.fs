module Bot

open Definitions

type t = player -> GameState.t -> GameState.t 
    (* let passBot *)
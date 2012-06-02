namespace Dominion
module Game =

  open Definitions
  open GameState

  let gameOver game = false

  let rec round (game : game_state) turnsTaken : game_state = 
    if turnsTaken >= Constants.TURN_LIMIT then game else
    let afterTurns = Utils.foldlSome game.players (fun game player -> 
      let afterTurn = player.bot game
      afterTurn, (if gameOver afterTurn then Utils.Stop else Utils.Continue)) game
    if gameOver afterTurns then afterTurns else round afterTurns (turnsTaken + 1)

  [<EntryPoint>]
  let main argv = 
     System.Console.WriteLine("Dominion!")
     printfn "Final state: %A" (round initialGameState 0)
     ignore(System.Console.ReadLine())
     0

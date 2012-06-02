namespace Dominion
module Game =

  open Definitions

  let gameOver gameState = gameState.turnsTaken >= Constants.TURN_LIMIT

  let rec round (gameState : gameState) = 
    let rec turn players gameState playerId =
      if gameOver gameState then gameState else
        match players with
          | [] -> gameState
          | hd::tl ->
                      let afterTurn = hd.bot playerId gameState |> GameState.nextTurn
                      turn tl afterTurn (playerId + 1)
    let afterRound = turn gameState.players gameState 0
    if gameOver afterRound then afterRound else round afterRound

  [<EntryPoint>]
  let main argv = 
     System.Console.WriteLine("Dominion!")
     printfn "Final state: %A" (round GameState.initialGameState)
     ignore(System.Console.ReadLine())
     0

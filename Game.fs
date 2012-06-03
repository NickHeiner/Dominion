namespace Dominion
module Game =

  open Definitions
  open Constants

  let gameOver gameState = gameState.turnsTaken >= Constants.TURN_LIMIT
                            || Map.find (Victory Province) gameState.cards = 0
                            || Map.filter (fun _ count -> count = 0) gameState.cards |> Map.toList |> List.length >= 3

  let score player = 
    let allCards = player.hand @ player.discard @ player.deck
    let gardensCount = List.sumBy (function Victory Gardens -> 1 | _ -> 0) allCards
    (List.sumBy victoryPointsFor allCards) + gardensCount * (List.length allCards / GARDENS_FACTOR)
    
  let rec round (gameState : gameState) = 
    let rec turn players gameState playerId =
      if gameOver gameState then gameState else
        match players with
          | [] -> gameState
          | hd::tl ->
                      let afterTurn = hd.bot playerId gameState
                                        |> GameState.updatePlayer playerId (fun player -> GameState.discardAll player |> GameState.draw 5)
                                        |> GameState.nextTurn
                      turn tl afterTurn (playerId + 1)
    let afterRound = turn gameState.players gameState 0
    if gameOver afterRound then afterRound else round afterRound

  let getInitialState bots =
    bots
      |> List.unzip
      |> snd
      |> Utils.withIndices 
      |> List.fold
      (fun gameState (index, bot) ->
        let newPlayerWithBot = {Constants.initialPlayer with bot = bot}
        {gameState with players = newPlayerWithBot::gameState.players})
        GameState.initialGameState 

  let main argv = 
     System.Console.WriteLine("Dominion!")
     let finalState = Bot.bots |> getInitialState |> round 
     printfn "Final Scores:"
     finalState.players
       |> List.map score
       |> List.zip (List.map fst Bot.bots)
       |> Utils.withIndices 
       |> List.sortBy snd
       |> List.rev
       |> List.iter (fun pair -> printfn "%A" pair)
     ignore(System.Console.ReadLine())
     0

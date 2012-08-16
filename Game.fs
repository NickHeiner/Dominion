namespace Dominion
module Game =

  open Definitions
  open Constants

  let gameOver gameState = gameState.turnsTaken >= Constants.TURN_LIMIT
                            || Map.find (Victory Province) gameState.cards = 0
                            || Map.filter (fun _ count -> count = 0) gameState.cards |> Map.toList |> List.length >= 3

  let score player = 
    let cards = Utils.allCards player
    let gardensCount = Utils.countOccurences cards (Victory Gardens)
    (List.sumBy victoryPointsFor cards) + gardensCount * (List.length cards / GARDENS_FACTOR)
    
  let rec round (gameState : gameState) = 
    let rec turn players gameState ((PId index) as pId) =
      if gameOver gameState then gameState else
        match players with
          | [] -> gameState
          | hd::tl ->
                      let rec applyUpdate apply bot gameState =
                        let afterUpdate = apply pId bot gameState
                        if afterUpdate = gameState then gameState else applyUpdate apply bot afterUpdate
                      let acts, buys = hd.bot
                      let afterTurn = applyUpdate BotHandler.GameStateUpdate.applyFirstValidAction acts gameState
                                        |> applyUpdate BotHandler.GameStateUpdate.applyFirstValidBuy buys
                                        |> GameState.updatePlayer pId (fun player -> GameState.discardAll player |> GameState.draw 5)
                                        |> GameState.nextTurn
                      turn tl afterTurn <| PId (index + 1)
    let afterRound = turn gameState.players gameState <| PId 0
    if gameOver afterRound then afterRound else round afterRound

  (* It's necessary to pick action cards that are in the game
     It would be good to look at the bots that are playing and see which cards they require. *)
  let getInitialState bots =
    bots
      |> List.rev
      |> List.unzip
      |> snd
      |> Utils.withIndices 
      |> List.fold
      (fun gameState (index, bot) ->
        let newPlayerWithBot = {Constants.initialPlayer with bot = bot}
        {gameState with players = newPlayerWithBot::gameState.players})
        (GameState.withCards STARTING_CARDS GameState.initialGameState)

  let playGame () = Bot.bots |> getInitialState |> round 

  let playGames () =
    Async.Parallel [ for i in 0..GAMES_TO_PLAY  -> async { return playGame () } ]
    |> Async.RunSynchronously
    |> Array.toList

  type playerStats = {name: string; score: float; cardCounts : Map<card, int>}

  let cardCountsOfPlayer player = 
    player
    |> Utils.allCards 
    |> Seq.ofList
    |> Seq.countBy (fun x -> x)
    |> Map.ofSeq

  let gameToPlayerStats bots game =
    List.map2 (fun player (name, _) -> {name = name; score = float <| score player; cardCounts = cardCountsOfPlayer player}) game.players bots
    |> List.sortBy (fun stats -> stats.score)
    |> List.rev

  let printPlayerStats = List.iter (fun playerStats -> printfn "%s\t%f\n%s\n" playerStats.name
                                                                playerStats.score
                                                                (Utils.prettyPrintCardCounts playerStats.cardCounts))

  let main argv = 
     printfn "Dominion!"
     printfn "Kicking off %d games..." GAMES_TO_PLAY
     let allStats = playGames ()
                 |> List.map (gameToPlayerStats Bot.bots)
     let averageStats =
        allStats
         |> List.fold (@) []
         |> Seq.groupBy (fun stats -> stats.name)
         |> Seq.map (fun (name, statsList) -> {name = name;
                                                score = Seq.averageBy (fun stats -> stats.score) statsList;
                                                cardCounts = Map.empty})
         |> Seq.toList

     printfn "__________Average Stats__________"
     printPlayerStats averageStats
     allStats
     |> List.iteri (fun index gameStats ->
        printfn "__________Final Scores (Game %d):__________" index
        printPlayerStats gameStats)
     ignore(System.Console.ReadLine())
     0
     
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
    Async.Parallel [ for i in 0..GAMES_TO_PLAY  -> async { let result = playGame ()
                                                           printf "."
                                                           return result } ]
    |> Async.RunSynchronously
    |> Array.toList

  type playerStats = {name: string; score: float; cardCounts : Map<card, float>}

  let cardCountsOfPlayer player = 
    player
    |> Utils.allCards 
    |> Seq.ofList
    |> Seq.countBy (fun x -> x)
    |> Seq.map (fun (card, count) -> card, float count)
    |> Map.ofSeq

  let gameToPlayerStats bots game =
    List.map2 (fun player (name, _) -> {name = name; score = float <| score player; cardCounts = cardCountsOfPlayer player}) game.players bots
    |> List.sortBy (fun stats -> stats.score)
    |> List.rev

  let printPlayerStats tabCount = 
      let tabs = List.replicate tabCount "\t"
                 |> List.fold (+) ""
      List.iter (fun playerStats -> printfn "%s%s\t%f\n%s%s\n" tabs 
                                                               playerStats.name
                                                               playerStats.score
                                                               tabs 
                                                               (Utils.prettyPrintCardCounts playerStats.cardCounts))
                                                               
  let aggregateCardCounts aggregate statsList = 
      statsList
        |> Seq.map (fun stats -> Map.toSeq stats.cardCounts)
        |> Seq.fold Seq.append Seq.empty
        |> Seq.groupBy fst
        |> Seq.map (fun (card, cardCountPairs) -> card, Seq.map snd cardCountPairs)
        |> aggregate
        |> Map.ofSeq

  let averageCardCounts = aggregateCardCounts
                          <| Seq.map (fun (card, countSeq) -> card, Seq.average countSeq)

  let minCardCounts statsList = aggregateCardCounts
                                <| Seq.map (fun (card, countSeq) -> card, Seq.average countSeq)

  
  let getScore stats = stats.score
  
  let main argv = 
     printfn "Dominion!"
     printfn "Kicking off %d games" GAMES_TO_PLAY
     let allStats = playGames () 
                    |> List.map (gameToPlayerStats Bot.bots)
     let statsByBot = 
        allStats
        |> List.fold (@) []
        |> Seq.groupBy (fun stats -> stats.name)

     let makeAggregateStats (name, getScore, getCardCounts) =
        name, (statsByBot 
                |> Seq.map (fun (name, statsSeq) -> {name = name;
                                                      score = getScore statsSeq;
                                                      cardCounts = getCardCounts statsSeq}))
     
     printfn ""

     let aggregates = ["Average", (Seq.averageBy getScore), averageCardCounts;
                       "Min", (fun statsSeq -> statsSeq |> Seq.map getScore |> Seq.min), averageCardCounts]
                      |> List.map makeAggregateStats
                      |> List.map (fun (name, seq) -> name, Seq.toList seq)

     aggregates
     |> List.map fst
     |> List.iter (printf "__________%s_____________\t")
     
     List.iteri (fun index (_, stats) -> printPlayerStats index stats) aggregates
                                        
     printfn "\n####################################################\n"
     allStats
     |> List.iteri (fun index gameStats ->
        printfn "__________Final Scores (Game %d):__________" index
        printPlayerStats 0 gameStats)
     ignore(System.Console.ReadLine())
     0
     
namespace Dominion
module Game =

  open Definitions
  open Constants

  let gameOver gameState = gameState.roundsPlayed >= Constants.ROUND_LIMIT
                            || Map.find (Victory Province) gameState.cards = 0
                            || Map.filter (fun _ count -> count = 0) gameState.cards |> Map.toList |> List.length >= 3

  let score player = 
    let cards = Utils.allCards player
    let gardensCount = Utils.countOccurences cards (Victory Gardens)
    (List.sumBy victoryPointsFor cards) + gardensCount * (List.length cards / GARDENS_FACTOR)

  let rec applyUpdate apply pId gameState =
    let afterUpdate = apply pId gameState
    if Utils.equalWithoutLog afterUpdate gameState then afterUpdate else applyUpdate apply pId afterUpdate

  let applyTurn gameState pId =
    if gameOver gameState then gameState else 
    gameState 
    |> applyUpdate BotHandler.GameStateUpdate.applyFirstValidAction pId 
    |> applyUpdate BotHandler.GameStateUpdate.applyFirstValidBuy pId 

  let applyTurn' pId gameState = applyTurn gameState pId

  let rec round initGame =
    if gameOver initGame then initGame else
    round {GameState.foldByPlayers applyTurn initGame with roundsPlayed = initGame.roundsPlayed + 1}

  let getInitialState bots =
    let actionCardsRequired = bots
                                |> BotHandler.actionCardsRequired 
                                |> Set.map (Action)
                                |> Set.toList
    
    if List.length actionCardsRequired > ACTION_CARDS_PER_GAME
        then invalidArg "bots" <| sprintf "Each game may only have %d action cards, but the chosen bots require %d: %A"
                                          ACTION_CARDS_PER_GAME
                                          (List.length actionCardsRequired)
                                          actionCardsRequired
      
    bots
    |> List.fold (fun gameState bot ->
                    let newPlayerWithBot = {GameState.initialPlayer () with bot = bot}
                    {gameState with players = newPlayerWithBot::gameState.players})
                 GameState.initialGameState   
    |> GameState.withCards (STARTING_CARDS @ actionCardsRequired)

  let playGame () = 
    let botNames = Bot.bots |> List.map (fun (name, _, _) -> name)
    if List.length botNames > (botNames |> Set.ofList |> Set.toList |> List.length)
        then invalidArg "bots" <| sprintf "Bots can't have duplicate names, but names were: %A" botNames

    Bot.bots |> getInitialState |> round 

  let playGames () =
    (* TODO Maybe we should validate the game (ie check count of action cards required) before launching the threads *)
    Async.Parallel [ for i in 0..GAMES_TO_PLAY - 1 -> async { let result = playGame ()
                                                              printf "."
                                                              return result } ]
    |> Async.RunSynchronously
    |> Array.toList

  let cardCountsOfPlayer = 
    Utils.allCards 
    >> Seq.ofList
    >> Seq.countBy (fun x -> x)
    >> Seq.map (fun (card, count) -> card, count)
    >> Map.ofSeq

  let gameToPlayerStats bots =
    GameState.getPlayers
    >> List.map (fun player -> let name, _, _ = player.bot
                               {name = name
                                score = score player
                                cardCounts = cardCountsOfPlayer player})
    >> List.sortBy (fun stats -> stats.score)
    >> List.rev

  let main _ = 
     printfn "Dominion!"
     printfn "Kicking off %d games" GAMES_TO_PLAY

     let finishedGames = playGames ()

     let gameResults =
        finishedGames 
        |> List.map (gameToPlayerStats Bot.bots)

     let placements = 
        gameResults
        |> List.map (fun games -> games
                                  |> List.sortBy (fun stats -> stats.score)
                                  |> List.rev)
        |> List.map (List.mapi (fun index playerStats -> playerStats.name, index))
        |> Seq.concat
        |> Seq.groupBy fst
        |> Seq.map (fun (name, placements) -> name, Seq.countBy snd placements |> Map.ofSeq)
        |> Map.ofSeq

     let workbook, analysisWorksheet = ExcelRenderer.makeWorksheet ()
     ExcelRenderer.addAnalysisData analysisWorksheet placements
     ExcelRenderer.addLog workbook finishedGames
     ExcelRenderer.addBotData workbook gameResults
     analysisWorksheet.Activate ()

     0
     
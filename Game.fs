namespace Dominion
module Game =

  open Definitions
  open Constants
  open Microsoft.Office.Interop.Excel

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

  let main argv = 
     printfn "Dominion!"
     printfn "Kicking off %d games" GAMES_TO_PLAY

     (* Excel documentation http://msdn.microsoft.com/en-us/library/hh297098.aspx *)
     let app = new ApplicationClass(Visible = true)
     let workbook = app.Workbooks.Add(XlWBATemplate.xlWBATWorksheet) 
     let worksheet = workbook.Worksheets.[1] :?> Worksheet
     worksheet.Name <- "Raw Data"

     playGames () 
     |> List.map (gameToPlayerStats Bot.bots)
     |> List.iteri (fun index gameStats ->
        let maxCardCountLength =
            gameStats
            |> List.map (fun stats -> stats.cardCounts |> Map.toList |> List.length)
            |> List.max
        let cards = 
            gameStats
            |> List.map (fun stats -> stats.cardCounts |> Map.toList)
            |> List.fold (@) []
            |> List.map fst
            |> Set.ofList
            |> Set.toArray
        let onlyHeader str = if index = 0 then str else ""
        let gameStatsArr = Array.ofList gameStats
        let cellContents =
            Array2D.init (ROW_OFFSET + List.length gameStats)
                         (COL_OFFSET + maxCardCountLength)
                         (fun row col -> match row, col with
                                         | 0, 0 -> box <| sprintf "Game %d" index
                                         | 0, 1 -> box <| onlyHeader "Score"
                                         | 0, c -> box <| onlyHeader (sprintf "%A" cards.[c - COL_OFFSET])
                                         | r, 0 -> box gameStatsArr.[r - ROW_OFFSET].name
                                         | r, 1 -> box gameStatsArr.[r - ROW_OFFSET].score 
                                         | r, c -> match Map.tryFind cards.[c - COL_OFFSET] gameStatsArr.[r - ROW_OFFSET].cardCounts with
                                                    | None -> box 0
                                                    | Some count -> box count) 

        let startRow = ((index * Array2D.length1 cellContents) + 1)
        
        let range = sprintf "A%d:%c%d" startRow
                                       ('A' + char(Array2D.length2 cellContents - 1))
                                       (startRow + Array2D.length1 cellContents - 1)

        worksheet.Range(range).Value2 <- cellContents
        )
     0
     
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
     let firstWorksheet = workbook.Worksheets.[1] :?> Worksheet
     firstWorksheet.Name <- "Analysis"
      
     let gameResults =
        playGames () 
        |> List.map (gameToPlayerStats Bot.bots)

     let placements = 
        gameResults
        |> List.map (fun games -> games
                                  |> List.sortBy (fun stats -> stats.score)
                                  |> List.rev)
        |> List.map (List.mapi (fun index playerStats -> playerStats.name, index))
        |> Utils.flatten
        |> List.toSeq
        |> Seq.groupBy fst
        |> Seq.map (fun (name, placements) -> name, Seq.countBy snd placements |> Map.ofSeq)
        |> Map.ofSeq

     let botCount = Map.toList placements |> List.length

     let cellEntries = 
        seq { for i in 0 .. botCount -> seq {for j in 0 .. botCount -> i, j}}
        |> Seq.concat
        |> Seq.map (fun coords -> coords, match coords with
                                          | 0, 0 -> box ""
                                          | row, 0 -> placements
                                                      |> Map.toList
                                                      |> List.map fst
                                                      |> Utils.nth (row - 1) (* -1 for headers *)
                                                      |> box
                                          | 0, col -> box col
                                          | row, col -> placements
                                                        |> Map.toList
                                                        |> List.map snd
                                                        |> Utils.nth (row - 1) (* -1 for headers *)
                                                        |> Utils.defaultFind (col - 1) 0 (* -1 b/c "1st, 2nd, 3rd" is 1-indexed, 
                                                                                            but placements is 0 indexed *)
                                                        |> box
                                          )
        |> Map.ofSeq

     let analysisRange = Utils.range (Row 0) (Col 0) (Row botCount) (Col botCount)
     firstWorksheet.Range(analysisRange).Value2 <- 
        (* +1 for header and labels *)
        Array2D.init (botCount + 1) (botCount + 1) (fun row col -> Map.find (row, col) cellEntries)

     // Add new item to the charts collection
     let chartobjects = (firstWorksheet.ChartObjects() :?> ChartObjects) 
     let chartobject = chartobjects.Add(400.0, 20.0, 550.0, 350.0) 

     // Configure the chart using the wizard
     chartobject.Chart.ChartWizard
       (Title = "Placements", 
        Source = firstWorksheet.Range(analysisRange),
        Gallery = XlChartType.xlColumnStacked, 
        PlotBy = XlRowCol.xlColumns)

     gameResults
     |> Utils.flatten
     |> Seq.groupBy (fun playerStats -> playerStats.name)
     |> Seq.iter (fun (name, statsSeq) ->
        (* This could all be done as a Map<int * int, obj> instead of imperatively, as it is here.
           That would be in the functional style of the rest of the program, and easier to test.
           However, it works already, so I'm just going to leave it. *)
        let worksheet = workbook.Worksheets.Add () :?> Worksheet
        worksheet.Name <- name

        let cardCounts = statsSeq
                        |> Seq.map (fun stats -> Map.toList stats.cardCounts)

        let cardCountsArr = cardCounts
                            |> Array.ofSeq
                            |> Array.map (fun statSeq -> statSeq |> Seq.map snd |> Array.ofSeq)

        let cardNames = cardCounts
                        |> Seq.toList
                        |> Utils.flatten
                        |> List.map fst
                        |> Set.ofList
                        |> Set.toArray
                        |> Array.map (sprintf "%A")

        (* TODO this isn't writing in score anywhere, and the "game %d" labels are in the 0th col *)

        worksheet.Range(Utils.singleCellRange (Row 0) (Col 1)).Value2 <- "Score"
        worksheet.Range(Utils.range (Row 0) (Col 2) (Row 0) (Col <| Array.length cardNames)).Value2 <- cardNames
        
        let maxCol = Array.length cardNames
        let nonLabelCols = seq { 0 .. maxCol - 1} (* -1 because upper bound in for loop is not exclusive *)

        statsSeq
        |> Seq.iteri (fun index stats -> 
            let row = index + 1 (* +1 to leave room for the header *)
            worksheet.Range(Utils.singleCellRange (Row row) (Col 0)).Value2 <- sprintf "Game %d" index
            worksheet.Range(Utils.singleCellRange (Row row) (Col 1)).Value2 <- stats.score
            for col in nonLabelCols do 
                let cell = Utils.singleCellRange (Row row) (Col (col + 2)) (* +1 to leave room for game label; +1 for score *)
                worksheet.Range(cell).Value2 <- cardCountsArr.[row - 1].[col]
            )
        
        let dataRows = Seq.length statsSeq
        let lastRow = dataRows + 2 (* +1 for header; +1 for spacing *)
        
        let setFormula formula row = for col in nonLabelCols do
                                        let formulaRange = Utils.range (Row row) (Col 1) (Row row) (Col maxCol)
                                        let sourceRange = Utils.range (Row 1) (Col col) (Row dataRows) (Col col)
                                        worksheet.Range(formulaRange).Value2 <- sprintf "=%s(%s)" formula sourceRange

        List.iteri
            (fun index (name, formula) -> worksheet.Range(Utils.singleCellRange (Row <| lastRow + index) (Col 0)).Value2 <- name
                                          setFormula formula <| lastRow + index)
            ["Mean", "average";
             "Min", "min";
             "Max", "max";
             "StdDev", "STDEV.P"] (* tbh I don't know which stddev formula is best *)
        )
        
     firstWorksheet.Activate ()                   
     0
     
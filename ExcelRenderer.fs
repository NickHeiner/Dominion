module ExcelRenderer
    
    (* Excel documentation http://msdn.microsoft.com/en-us/library/hh297098.aspx *)
    open Microsoft.Office.Interop.Excel
    open Definitions
    open Constants

    (* There is potential for sweet operator overloading for Row and Col *)
    type row = Row of int
    type col = Col of int

    let inline (+++) (Row r0) (Row r1) = Row <| r0 + r1

    (* Only handles columns up to ZZ.
       But there's no need to go beyond that now, so I'm going to leave it that way. *)
    let makeCell (Row row) (Col col) = 
        let charCol =
            let maxCol = int 'Z' - int 'A'
            if col <= maxCol
                then 'A' + char col |> string
                else ['A' + char (col / maxCol - 1); 'A' +  char (max (col % maxCol - 1) 0)]
                     |> List.map string
                     |> String.concat ""
        sprintf "%s%d" charCol (row + 1)

    let range startRow startCol endRow endCol = sprintf "%s:%s" (makeCell startRow startCol) (makeCell endRow endCol)
    let singleCellRange row col =
        let cell = makeCell row col
        sprintf "%s:%s" cell cell

    let render (worksheet : Worksheet) (Row startRowIndex) (Col startColIndex) =
        Map.iter (fun (Row rowIndex, Col colIndex) entry ->
                    let targetRange = singleCellRange (Row <| rowIndex + startRowIndex) (Col <| colIndex + startColIndex)
                    worksheet.Range(targetRange).Value2 <- entry)

    let makeWorksheet () = 
         let app = new ApplicationClass(Visible = true)
         let workbook = app.Workbooks.Add(XlWBATemplate.xlWBATWorksheet)
         let firstWorksheet = workbook.Worksheets.[1] :?> Worksheet
         firstWorksheet.Name <- "Analysis"
         workbook, firstWorksheet

    let sortPlacements placements =
        placements
        |> Map.toList
        |> List.sortBy (fun (_, places) -> seq { 0 .. (Map.toList placements |> List.length) - 1}
                                           |> Seq.map (fun i -> Utils.defaultFind i 0 places)
                                           |> Seq.toList)
        |> List.rev

    let botNameLabels placements = 
        placements
        |> sortPlacements
        |> List.mapi (fun index (name, _) -> ((Row index, Col 0), name))
        |> Map.ofList

    let placeLabels placements =
        seq { 0 .. (Map.toList placements |> List.length) - 1}
        |> Seq.map (fun place -> (Row 0, Col place), place + 1) (* +1 b/c place is 1-indexed *)
        |> Map.ofSeq

    let placeFreqs placements = 
        let botCount = placements
                        |> Map.toList
                        |> List.length
        let places = placements
                        |> sortPlacements
                        |> List.map snd
        seq { for i in 0 .. (botCount - 1) -> seq {for j in 0 .. (botCount - 1) -> i, j}}
        |> Seq.concat
        |> Seq.map (fun ((row, col) as coords) -> (Row row, Col col), places
                                                                        |> Utils.nth row
                                                                        |> Utils.defaultFind col 0)
        |> Map.ofSeq

    let addAnalysisData worksheet placements =
        render worksheet (Row 1) (Col 0) <| botNameLabels placements
        render worksheet (Row 0) (Col 1) <| placeLabels placements
        render worksheet (Row 1) (Col 1) <| placeFreqs placements

        let chartobjects = (worksheet.ChartObjects() :?> ChartObjects) 
        let chartobject = chartobjects.Add(400.0, 20.0, 550.0, 350.0) 

        let botCount = placements |> Map.toList |> List.length
        let sourceRange = range (Row 0) (Col 0) (Row botCount) (Col botCount)

        // Configure the chart using the wizard
        chartobject.Chart.ChartWizard
           (Title = "Placements", 
            Source = worksheet.Range(sourceRange),
            Gallery = XlChartType.xlColumnStacked, 
            PlotBy = XlRowCol.xlRows)

    let cardNamesOf statsSeq =
        statsSeq
        |> Seq.map (fun stats -> Map.toList stats.cardCounts)
        |> Seq.toList
        |> Seq.concat
        |> Seq.map fst
        |> Set.ofSeq
        |> Set.toList
        |> List.mapi (fun index card -> (Row 0, Col index), sprintf "%A" card)
        |> Map.ofList

    let gameLabelsOf stats =
        stats
        |> Seq.mapi (fun index _ -> (Row index, Col 0), sprintf "Game %d" index)
        |> Map.ofSeq

    let allCards = 
        Seq.map (fun stats -> Map.toSeq stats.cardCounts)
        >> Seq.concat
        >> Seq.map fst
        >> Seq.toList
        >> List.sort (* We must sort, because the card headers are done in sorted order *)
        >> Set.ofList (* Get rid of duplicates *)
        >> Set.toList

    let cardCountsOf (stats : seq<playerStats>) = 
        let allCardsList = allCards stats
        
        stats
        |> Seq.mapi
            (fun gameIndex gameResults ->
                Seq.mapi (fun cardIndex card -> (Row gameIndex, Col cardIndex), Utils.defaultFind card 0. gameResults.cardCounts) allCardsList)
        |> Seq.concat
        |> Map.ofSeq

    let scoresOf stats = 
        stats 
        |> Seq.mapi (fun index gameResults -> (Row index, Col 0), gameResults.score)
        |> Map.ofSeq

    let aggrLabelsOf  = 
        List.mapi (fun index (name, _) -> (Row index, Col 0), name)
        >> Map.ofList

    let aggrFormulasOf stats statsOutput ((Row dataRowStart) as rowStart) (Col dataColStart) =
        let colCount = (allCards stats |> List.length) + 1 (* +1 for score column *)
        let rowCount = Seq.length stats
        let formulae = List.map snd statsOutput
        let lastRow = Row <| rowCount + dataRowStart
        seq {for row in 0 .. List.length statsOutput - 1 do
                for col in 0 .. colCount - 1 do
                    let currCol = Col <| col + dataColStart
                    let sourceRange = range rowStart currCol
                                            lastRow  currCol
                    yield (Row row, Col col), sprintf "=%s(%s)" (List.nth formulae row) sourceRange
            }
        |> Map.ofSeq

    let addBotData (workbook : Workbook) =
        Seq.concat
        >> Seq.groupBy (fun playerStats -> playerStats.name)
        >> Seq.iter (fun (name, stats) ->
            let dataRowCount = Seq.length stats 
            let dataRowStart = Row 1
            let dataColStart = Col 1
            let worksheet = workbook.Worksheets.Add () :?> Worksheet
            worksheet.Name <- name

            let renderWithOffset ((row, col), entries) = render worksheet row col entries

            let aggr = aggrFormulasOf stats STATS_OUTPUT dataRowStart dataColStart
            let aggrRow = dataRowStart +++ (Row dataRowCount) +++ (Row AGGR_EMPTY_LINES)

            (* We have to do these separately because they return strings numbers instead of strings *)
            [(Row 1, Col 2), cardCountsOf stats;
             (Row 1, Col 1), scoresOf stats]
            |> List.iter renderWithOffset

            [(Row 0, Col 1), Map.ofList [(Row 0, Col 0), "Score"];
             (Row 0, Col 2), cardNamesOf stats;
             (Row 1, Col 0), gameLabelsOf stats;
             (aggrRow, Col 0), aggrLabelsOf STATS_OUTPUT;
             (aggrRow, Col 1), aggr]
            |> List.iter renderWithOffset)

    let unpackEvent = function
        | Act card -> "Act", Utils.toString card
        | Buy card -> "Buy", Utils.toString card
        | PassAct -> "Pass", "Act"
        | PassBuy -> "Pass", "Buy"
    
    let getLogCells nameLookup =
        List.mapi (fun gameId -> List.map (fun entry -> gameId, entry) >> List.rev)
        >> Seq.concat
        >> Seq.mapi (fun rowIndex (gameId, logEntry) -> 
                        let row = Row rowIndex
                        let eventName, eventArg = unpackEvent logEntry.event
                        [Utils.toString gameId
                         Utils.toString logEntry.round
                         Map.find logEntry.pId nameLookup
                         eventName
                         eventArg
                         logEntry.currHand |> List.map Utils.toString |> String.concat ", "
                         Utils.toString logEntry.turn.actions
                         Utils.toString logEntry.turn.buys
                         Utils.toString logEntry.turn.purchasingPower]
                        |> List.mapi (fun index contents -> (row, Col index), contents))
        >> Seq.concat
        >> Map.ofSeq

    let addLog (workbook : Workbook) games = 
        let worksheet = workbook.Worksheets.Add () :?> Worksheet
        worksheet.Name <- "Log"

        ["Game Id"
         "Round Id"
         "Player"
         "Event"
         "Arg"
         "Hand"
         "Actions"
         "Buys"
         "Purchasing Power"]
         |> List.mapi (fun index header -> (Row 0, Col index), header)
         |> Map.ofList
         |> render worksheet (Row 0) (Col 0)
       
        let nameLookup =
            match games with
            |   hd::_ -> hd.players
                         |> List.mapi (fun index player -> (PId index, Utils.fst3 player.bot))
                         |> Map.ofList
            |   [] -> Map.empty
        
        render worksheet (Row 1) (Col 0) <| getLogCells nameLookup (List.map (fun game -> game.log) games)




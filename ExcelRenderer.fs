module ExcelRenderer
    
    (* Excel documentation http://msdn.microsoft.com/en-us/library/hh297098.aspx *)
    open Microsoft.Office.Interop.Excel
    open Definitions
    open Constants

    (* It's not obvious that these should be classes instead of plain structural types *)
    type Row (x: int) =
        let getRowIndex (r : Row) = r.x
        member this.x = x
        static member (+) (r : Row, r' : Row) = Row (r.x + r'.x)
        static member (-) (r : Row, r' : Row) = Row (r.x - r'.x)
        static member One = Row 1
        interface System.IComparable with
            member r.CompareTo yobj = Utils.compareOn getRowIndex r yobj
        override r.Equals yobj = Utils.equalsOn getRowIndex r yobj
        override r.GetHashCode () = Utils.hashOn getRowIndex r

    type Col (y: int) =
        let getColIndex (c : Col) = c.y
        member this.y = y
        static member (+) (c : Col, c' : Col) = Col (c.y + c'.y)
        static member (-) (c : Col, c' : Col) = Col (c.y - c'.y)
        static member One = Col 1
        interface System.IComparable with
            member c.CompareTo yobj = Utils.compareOn getColIndex c yobj
        override c.Equals yobj = Utils.equalsOn getColIndex c yobj
        override c.GetHashCode () = Utils.hashOn getColIndex c

    (* Only handles columns up to ZZ.
       But there's no need to go beyond that now, so I'm going to leave it that way. *)
    let makeCell (row : Row) (col : Col) = 
        let charCol =
            let maxCol = int 'Z' - int 'A'
            if col.y <= maxCol
                then 'A' + char col.y |> string
                else ['A' + char (col.y / maxCol - 1); 'A' +  char (max (col.y % maxCol - 1) 0)]
                     |> List.map string
                     |> String.concat ""
        sprintf "%s%d" charCol (row.x + 1)

    let range startRow startCol endRow endCol = sprintf "%s:%s" (makeCell startRow startCol) (makeCell endRow endCol)
    let singleCellRange row col =
        let cell = makeCell row col
        sprintf "%s:%s" cell cell

    let getCellBounds cells =
        Map.fold (fun (minRow, minCol, maxRow, maxCol) (row, col) _ -> min minRow row,
                                                                       min minCol col,
                                                                       max maxRow row,
                                                                       max maxCol col)
                 (Row System.Int32.MaxValue, Col System.Int32.MaxValue, Row 0, Col 0)
                 cells

    let cellDataOfCells cells = 
        let _, _, maxRow, maxCol = getCellBounds cells
        Array2D.init (maxRow + Row 1).x
                     (maxCol + Col 1).y
                     (fun rowIndex colIndex -> Map.find ((Row rowIndex), (Col colIndex)) cells)

    let verifyContinuous cells =
        let minRow, minCol, maxRow, maxCol = getCellBounds cells
        if not <| (minRow = Row 0 && minCol = Col 0) then invalidArg "cells" "Cells must start at (0, 0)" else
        if not (seq { for row in minRow .. maxRow do
                         for col in minCol .. maxCol do
                            yield row, col}
                |> Seq.forall (fun key -> Map.containsKey key cells))
        then invalidArg "cells" "Cells must contain content for each cell within its bounds"

    let render (worksheet : Worksheet) (startRow : Row) (startCol : Col) cells =
        verifyContinuous cells
        let minRow, minCol, maxRow, maxCol = getCellBounds cells
        let targetRange = range (minRow + startRow) (minCol + startCol) (maxRow + startRow) (maxCol + startCol)
        let cellData = cellDataOfCells cells
        worksheet.Range(targetRange).Value2 <- cellData

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
                Seq.mapi (fun cardIndex card -> (Row gameIndex, Col cardIndex), Utils.defaultFind card 0 gameResults.cardCounts) allCardsList)
        |> Seq.concat
        |> Map.ofSeq

    let scoresOf stats = 
        stats 
        |> Seq.mapi (fun index gameResults -> (Row index, Col 0), gameResults.score)
        |> Map.ofSeq

    let aggrLabelsOf  = 
        List.mapi (fun index (name, _) -> (Row index, Col 0), name)
        >> Map.ofList

    let aggrFormulasOf stats statsOutput dataRowStart dataColStart =
        let colCount = (allCards stats |> List.length) + 1 (* +1 for score column *)
        let rowCount = Seq.length stats
        let formulae = List.map snd statsOutput
        let lastRow = Row rowCount + dataRowStart
        seq {for row in 0 .. List.length statsOutput - 1 do
                for col in 0 .. colCount - 1 do
                    let currCol = Col col + dataColStart
                    let sourceRange = range dataRowStart currCol lastRow currCol
                    yield (Row row, Col col), box <| sprintf "=%s(%s)" (List.nth formulae row) sourceRange
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
            let aggrRow = dataRowStart + (Row dataRowCount) + (Row AGGR_EMPTY_LINES)

            (* We have to do these separately because they return strings numbers instead of strings *)
            [(Row 1, Col 2), cardCountsOf stats
             (Row 1, Col 1), scoresOf stats]
            |> List.iter renderWithOffset

            [(Row 0, Col 1), Map.ofList [(Row 0, Col 0), "Score"]
             (Row 0, Col 2), cardNamesOf stats
             (Row 1, Col 0), gameLabelsOf stats
             (aggrRow, Col 0), aggrLabelsOf STATS_OUTPUT]
            |> List.iter renderWithOffset
            
            render worksheet (aggrRow) (Col 1) aggr)

    let unpackEvent = function
        | Act card -> "Act", Utils.toString card
        | Buy card -> "Buy", Utils.toString card
        | PassAct -> "Pass", "Act"
        | PassBuy -> "Pass", "Buy"
    
    let cellOfContents row index contents = (row, Col index), contents

    let entryToRow nameLookup rowIndex (gameId, logEntry) = 
        let row = Row rowIndex
        let eventName, eventArg = unpackEvent logEntry.event
        ([gameId
          logEntry.round
          Map.find logEntry.pId nameLookup
          eventName
          eventArg
          logEntry.currHand |> List.map Utils.toString |> String.concat ", "
          logEntry.turn.actions
          logEntry.turn.buys
          logEntry.turn.purchasingPower] : obj list)
        |> Seq.mapi (cellOfContents row)

    let getLogCells nameLookup =
        List.mapi (fun gameId -> List.map (fun entry -> gameId, entry) >> List.rev)
        >> Seq.concat
        >> Seq.mapi (entryToRow nameLookup)
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
        
        render worksheet (Row 1) (Col 0) <| getLogCells nameLookup (List.map GameState.getLog games)




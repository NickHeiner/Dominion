﻿module ExcelRenderer
    
    (* Excel documentation http://msdn.microsoft.com/en-us/library/hh297098.aspx *)
    open Microsoft.Office.Interop.Excel
    open Definitions
    open Constants

    (* There is potential for sweet operator overloading for Row and Col *)
    type row = Row of int
    type col = Col of int

    let makeCell (Row row) (Col col) = sprintf "%c%d" ('A' + char col) (row + 1)
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
        |> Utils.flatten
        |> List.map fst
        |> Set.ofList
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
        Utils.flatten
        >> Seq.groupBy (fun playerStats -> playerStats.name)
        >> Seq.iter (fun (name, stats) ->
            let lastRow = Seq.length stats 
            let dataRowStart = Row 1
            let dataColStart = Col 1
            let worksheet = workbook.Worksheets.Add () :?> Worksheet
            worksheet.Name <- name

            let renderWithOffset ((row, col), entries) = render worksheet row col entries

            [(Row 0, Col 1), Map.ofList [(Row 0, Col 0), "Score"];
             (Row 0, Col 2), cardNamesOf stats;
             (Row 1, Col 0), gameLabelsOf stats;
             (Row lastRow, Col 0), aggrLabelsOf STATS_OUTPUT;
             (Row lastRow, Col 1), aggrFormulasOf stats STATS_OUTPUT dataRowStart dataColStart]
            |> List.iter renderWithOffset

            (* We have to do these separately because they return strings numbers instead of strings *)
            [(Row 1, Col 2), cardCountsOf stats;
             (Row 1, Col 1), scoresOf stats]
            |> List.iter renderWithOffset)



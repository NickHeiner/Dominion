module ExcelRenderer
    
    (* Excel documentation http://msdn.microsoft.com/en-us/library/hh297098.aspx *)
    open Microsoft.Office.Interop.Excel
    
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

    let sortPlacements placements = placements |> Map.toList |> List.sortBy snd

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
module Football.Tests.ResultsReader

open FSharp.Data
open FSharp.Data.CsvExtensions

open Football

let readRow (row: CsvRow) =
    {
        HomeTeam = row?HomeTeam |> Team
        AwayTeam = row?AwayTeam |> Team
        HomeScore = row?FTHG.AsInteger()
        AwayScore = row?FTAG.AsInteger()
    }

let readCsv (filename: string) =
    let resultsFile = CsvFile.Load(filename)

    resultsFile.Rows
    |> Seq.map readRow
    |> Seq.toList

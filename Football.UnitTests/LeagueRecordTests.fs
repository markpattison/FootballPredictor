namespace Football.Tests

open FsUnit
open NUnit.Framework

open Football

[<TestFixture>]
module LeagueRecordTests =

    let getAllTeams results =
        results
        |> List.collect (fun r -> [ r.HomeTeam; r.AwayTeam ])
        |> List.distinct

    [<Test>]
    let ``league table after one game`` ()=
        let team1 = Team "Team1"
        let team2 = Team "Team2"

        let teams = [ team1; team2 ]

        let m =
            {
                HomeTeam = team1
                AwayTeam = team2
                HomeScore = 2
                AwayScore = 1
            }

        let leagueTable = League.compileAllRecords teams [m]

        leagueTable |> (should haveLength 2)

        (leagueTable.Item 0) |> should equal
            {
                Team = team1
                Won = 1
                Drawn = 0
                Lost = 0
                GoalsFor = 2
                GoalsAgainst = 1
            }

        (leagueTable.Item 1) |> should equal
            {
                Team = team2
                Won = 0
                Drawn = 0
                Lost = 1
                GoalsFor = 1
                GoalsAgainst = 2
            }

    [<Test>]
    let ``premier league 2016-17 after ten games`` ()=
        let results = ResultsReader.readCsv @"E:\Prog\Visual Studio 2015\Projects\FootballPredictor\Football.UnitTests\data\prem201617 up to 30 Oct.csv"
        let teams = getAllTeams results

        let leagueTable =
            League.compileAllRecords teams results
            |> League.order

        (leagueTable.Item 0) |> should equal
            {
                Team = Team "Man City"
                Won = 7
                Drawn = 2
                Lost = 1
                GoalsFor = 24
                GoalsAgainst = 9
            }

        (leagueTable.Item 19) |> should equal
            {
                Team = Team "Sunderland"
                Won = 0
                Drawn = 2
                Lost = 8
                GoalsFor = 7
                GoalsAgainst = 20
            }

    [<Test>]
    let ``premier league 2016-17 after ten games - remaining fixtures`` ()=
        let results = ResultsReader.readCsv @"E:\Prog\Visual Studio 2015\Projects\FootballPredictor\Football.UnitTests\data\prem201617 up to 30 Oct.csv"
        let teams = getAllTeams results

        let remainingFixtures = League.remainingFixtures teams results

        remainingFixtures |> should haveLength 281

    [<Test>]
    let ``a test`` ()=
        let results = ResultsReader.readCsv @"E:\Prog\Visual Studio 2015\Projects\FootballPredictor\Football.UnitTests\data\prem201617 up to 30 Oct.csv"
        let teams = getAllTeams results

        let results = Projection.projections teams results

        1 |> should equal 1
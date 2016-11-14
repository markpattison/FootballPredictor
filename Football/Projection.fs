module Football.Projection

let singleMatch fixture =
    {
        HomeTeam = fixture.Home
        AwayTeam = fixture.Away
        HomeScore = Poisson.getNext 2.5
        AwayScore = Poisson.getNext 2.0
    }

let singleProjection fixtures =
    fixtures
    |> List.map singleMatch

let findRecord team l =
    l |> List.find (fun lr -> lr.Team = team)

let projections teams results =
    let recordsSoFar = League.compileAllRecords teams results

    let remainingFixtures = League.remainingFixtures teams results

    let simulations = 5000

    let finalTables = 
        [1 .. simulations]
        |> List.map (fun _ ->
            let futureResults = singleProjection remainingFixtures
            let futureRecords = League.compileAllRecords teams futureResults
            let finalRecords =
                teams
                |> List.map (fun team -> (findRecord team recordsSoFar) + (findRecord team futureRecords))
                |> League.order
            finalRecords)

    let howManyChampions team =
        finalTables
        |> List.filter (fun table -> (table.Item 0).Team = team)
        |> List.length

    let championProportions =
        teams
        |> List.map (fun team -> (team, float (howManyChampions team) / float simulations))
        |> List.sortByDescending snd

    championProportions


    


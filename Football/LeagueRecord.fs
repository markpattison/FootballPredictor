namespace Football

type LeagueRecord =
    {
        Team: Team
        Won: int
        Drawn: int
        Lost: int
        GoalsFor: int
        GoalsAgainst: int
    }
    member this.Played = this.Won + this.Drawn + this.Lost
    member this.GoalDifference = this.GoalsFor - this.GoalsAgainst
    member this.Points = 3 * this.Won + this.Drawn
    static member (+) (a, b) =
        {
            Team = if a.Team = b.Team then a.Team else failwith "different teams"
            Won = a.Won + b.Won
            Drawn = a.Drawn + b.Drawn
            Lost = a.Lost + b.Lost
            GoalsFor = a.GoalsFor + b.GoalsFor
            GoalsAgainst = a.GoalsAgainst + b.GoalsAgainst
        }

module League =

    type TeamResult = | Won | Drawn | Lost

    type TeamMatchResult =
        {
            TeamResult: TeamResult
            GoalsScored: int
            GoalsConceded: int
        }

    let teamResult location result =
        let getResult netScore =
            match netScore with
            | x when x > 0 -> Won
            | x when x < 0 -> Lost
            | _ -> Drawn
        match location with
        | Home ->
            {
                TeamResult = getResult (result.HomeScore - result.AwayScore)
                GoalsScored = result.HomeScore
                GoalsConceded = result.AwayScore
            }
        | Away ->
            {
                TeamResult = getResult (result.AwayScore - result.HomeScore)
                GoalsScored = result.AwayScore
                GoalsConceded = result.HomeScore
            }

    let howMany f l =
        l |> List.filter f |> List.length

    let compileRecord results team =
        let homeResults =
            results
            |> List.filter (fun m -> m.HomeTeam = team)
            |> List.map (teamResult Home)
        let awayResults =
            results
            |> List.filter (fun m -> m.AwayTeam = team)
            |> List.map (teamResult Away)

        let results = List.append homeResults awayResults

        {
            Team = team
            Won = results |> howMany (fun r -> r.TeamResult = Won)
            Drawn = results |> howMany (fun r -> r.TeamResult = Drawn)
            Lost = results |> howMany (fun r -> r.TeamResult = Lost)
            GoalsFor = results |> List.sumBy (fun r -> r.GoalsScored)
            GoalsAgainst = results |> List.sumBy (fun r -> r.GoalsConceded)
        }

    let inline (|?) a b = if a = 0 then b else a

    let compare (lr1: LeagueRecord) (lr2: LeagueRecord) =
        (lr2.Points - lr1.Points)
        |? (lr2.GoalDifference - lr1.GoalDifference)
        |? (lr2.GoalsFor - lr1.GoalsFor)
        |? System.String.Compare(lr2.Team.Name, lr1.Team.Name)

    let compileAllRecords teams results =
        teams
        |> List.map (compileRecord results)

    let remainingFixtures teams results =
        let remainingFixturesForHomeTeam homeTeam =
            let alreadyPlayed =
                results
                |> List.filter (fun r -> r.HomeTeam = homeTeam)
                |> List.map (fun r -> r.AwayTeam)
            let notLeftToPlay = homeTeam :: alreadyPlayed
            teams
            |> List.except notLeftToPlay
            |> List.map (fun t -> { Home = homeTeam; Away = t })
        teams
        |> List.collect remainingFixturesForHomeTeam

    let order l =
        l
        |> List.sortWith compare

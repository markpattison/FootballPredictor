namespace Football

type Team =
    | Team of string
    member this.Name =
        match this with
        | Team name -> name

type Location = | Home | Away

type MatchType =
    {
        HomeTeam: Team
        AwayTeam: Team
        HomeScore: int
        AwayScore: int
    }

type Fixture =
    {
        Home: Team
        Away: Team
    }
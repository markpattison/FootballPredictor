module Football.Random

let random = new System.Random()

let getPoisson lambda =
    let l = exp(-lambda)
    let rec step k p =
        let u = random.NextDouble()
        let p' = p * u
        if p > l then
            step (k + 1) p'
        else
            k
    step -1 1.0

let getGeometric p =
    let u = random.NextDouble()
    let g = floor ((log u) / (log (1.0 - p)))
    (int) g

let getNegativeBinomial r p =
    [1 .. r] |> List.sumBy (fun _ -> getGeometric (1.0 - p))
    


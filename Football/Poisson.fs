module Football.Poisson

let random = new System.Random()

let getNext lambda =
    let l = exp(-lambda)
    let rec step k p =
        let u = random.NextDouble()
        let p' = p * u
        if p > l then
            step (k + 1) p'
        else
            k
    step -1 1.0



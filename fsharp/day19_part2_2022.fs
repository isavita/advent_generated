
open System
open System.IO
open System.Text.RegularExpressions

type Blueprint = { 
    OreCost: int; ClayOreCost: int; 
    ObsidianOreCost: int; ObsidianClayCost: int; 
    GeodeOreCost: int; GeodeObsidianCost: int; 
    MaxOre: int 
}

let solve b maxTime =
    let mutable best = 0
    let rec dfs (ore, clay, obs, geo) (rOre, rClay, rObs, rGeo) t =
        let total = geo + rGeo * t
        if total > best then best <- total
        
        if t > 1 then
            if total + (t * (t - 1)) / 2 > best then
                if rObs > 0 then
                    let w = max 0 (max ((b.GeodeOreCost - ore + rOre - 1) / rOre) ((b.GeodeObsidianCost - obs + rObs - 1) / rObs))
                    let wait = w + 1
                    if t - wait > 0 then
                        dfs (ore + rOre * wait - b.GeodeOreCost, clay + rClay * wait, obs + rObs * wait - b.GeodeObsidianCost, geo + rGeo * wait)
                            (rOre, rClay, rObs, rGeo + 1) (t - wait)

                if rClay > 0 && rObs < b.GeodeObsidianCost then
                    let w = max 0 (max ((b.ObsidianOreCost - ore + rOre - 1) / rOre) ((b.ObsidianClayCost - clay + rClay - 1) / rClay))
                    let wait = w + 1
                    if t - wait > 1 then
                        dfs (ore + rOre * wait - b.ObsidianOreCost, clay + rClay * wait - b.ObsidianClayCost, obs + rObs * wait, geo + rGeo * wait)
                            (rOre, rClay, rObs + 1, rGeo) (t - wait)

                if rClay < b.ObsidianClayCost then
                    let w = max 0 ((b.ClayOreCost - ore + rOre - 1) / rOre)
                    let wait = w + 1
                    if t - wait > 2 then
                        dfs (ore + rOre * wait - b.ClayOreCost, clay + rClay * wait, obs + rObs * wait, geo + rGeo * wait)
                            (rOre, rClay + 1, rObs, rGeo) (t - wait)

                if rOre < b.MaxOre then
                    let w = max 0 ((b.OreCost - ore + rOre - 1) / rOre)
                    let wait = w + 1
                    if t - wait > 2 then
                        dfs (ore + rOre * wait - b.OreCost, clay + rClay * wait, obs + rObs * wait, geo + rGeo * wait)
                            (rOre + 1, rClay, rObs, rGeo) (t - wait)
    
    dfs (0, 0, 0, 0) (1, 0, 0, 0) maxTime
    best

[<EntryPoint>]
let main _ =
    let input = File.ReadAllLines("input.txt")
    let blueprints = [|
        for line in input do
            let m = Regex.Match(line, @"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.")
            if m.Success then
                let b = { 
                    OreCost = int m.Groups.[2].Value; ClayOreCost = int m.Groups.[3].Value; 
                    ObsidianOreCost = int m.Groups.[4].Value; ObsidianClayCost = int m.Groups.[5].Value; 
                    GeodeOreCost = int m.Groups.[6].Value; GeodeObsidianCost = int m.Groups.[7].Value; 
                    MaxOre = 0
                }
                yield { b with MaxOre = List.max [b.OreCost; b.ClayOreCost; b.ObsidianOreCost; b.GeodeOreCost] }
    |]
    
    let mutable ans = 1L
    let count = min 3 blueprints.Length
    for i in 0 .. count - 1 do
        ans <- ans * int64 (solve blueprints.[i] 32)
    printfn "%d" ans
    0

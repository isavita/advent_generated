
open System
open System.IO

type Reindeer =
    { Speed:int
      FlyTime:int
      RestTime:int
      mutable Distance:int
      mutable Points:int
      mutable Flying:bool
      mutable TimeInMode:int }

[<EntryPoint>]
let main argv =
    let lines = File.ReadAllLines "input.txt"
    let reindeers =
        lines
        |> Array.map (fun line ->
            let parts = line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
            let speed = Int32.Parse(parts.[3])
            let flyTime = Int32.Parse(parts.[6])
            let restTime = Int32.Parse(parts.[13])
            { Speed = speed
              FlyTime = flyTime
              RestTime = restTime
              Distance = 0
              Points = 0
              Flying = true
              TimeInMode = 0 })
    let totalSeconds = 2503
    for _ = 1 to totalSeconds do
        let mutable maxDist = 0
        for r in reindeers do
            if r.Flying then r.Distance <- r.Distance + r.Speed
            r.TimeInMode <- r.TimeInMode + 1
            if (r.Flying && r.TimeInMode = r.FlyTime) || (not r.Flying && r.TimeInMode = r.RestTime) then
                r.Flying <- not r.Flying
                r.TimeInMode <- 0
            if r.Distance > maxDist then maxDist <- r.Distance
        for r in reindeers do
            if r.Distance = maxDist then r.Points <- r.Points + 1
    let maxPoints = reindeers |> Array.maxBy (fun r -> r.Points) |> fun r -> r.Points
    printfn "%d" maxPoints
    0

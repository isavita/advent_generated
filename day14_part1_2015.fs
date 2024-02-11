
module Day14

open System.IO

let lines = File.ReadAllLines("input.txt")

type Reindeer = {
    name: string
    speed: int
    flyTime: int
    restTime: int
}

let parseLine (line: string) =
    let parts = line.Split(' ')
    {
        name = parts.[0]
        speed = int parts.[3]
        flyTime = int parts.[6]
        restTime = int parts.[13]
    }

let reindeers = lines |> Array.map parseLine

let distanceTraveled (reindeer: Reindeer) time =
    let cycleTime = reindeer.flyTime + reindeer.restTime
    let cycles = time / cycleTime
    let remainingTime = time % cycleTime
    let flyDistance = min reindeer.flyTime remainingTime * reindeer.speed
    cycles * reindeer.flyTime * reindeer.speed + flyDistance

let maxDistance = reindeers |> Array.map (fun r -> distanceTraveled r 2503) |> Array.max

printfn "%d" maxDistance

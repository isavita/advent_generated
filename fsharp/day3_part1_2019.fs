open System
open System.IO
open System.Collections.Generic

let addPath (path:string) (set:HashSet<int*int>) =
    let mutable x = 0
    let mutable y = 0
    for token in path.Split([|','|], StringSplitOptions.RemoveEmptyEntries) do
        let dir = token.[0]
        let steps = Int32.Parse(token.Substring(1))
        for _ in 1..steps do
            match dir with
            | 'U' -> y <- y + 1
            | 'D' -> y <- y - 1
            | 'L' -> x <- x - 1
            | 'R' -> x <- x + 1
            | _ -> ()
            set.Add((x, y)) |> ignore

let findMinDist (path:string) (visited:HashSet<int*int>) =
    let mutable x = 0
    let mutable y = 0
    let mutable minDist = Int32.MaxValue
    for token in path.Split([|','|], StringSplitOptions.RemoveEmptyEntries) do
        let dir = token.[0]
        let steps = Int32.Parse(token.Substring(1))
        for _ in 1..steps do
            match dir with
            | 'U' -> y <- y + 1
            | 'D' -> y <- y - 1
            | 'L' -> x <- x - 1
            | 'R' -> x <- x + 1
            | _ -> ()
            if visited.Contains((x, y)) then
                let dist = Math.Abs(x) + Math.Abs(y)
                if dist < minDist then minDist <- dist
    minDist

[<EntryPoint>]
let main argv =
    let lines = File.ReadAllLines("input.txt")
    if lines.Length < 2 then 0 else
    let visited = HashSet<int*int>()
    addPath (lines.[0].Trim()) visited
    let result = findMinDist (lines.[1].Trim()) visited
    printfn "%d" result
    0
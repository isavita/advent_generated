
module Day25

open System.IO

let parseInput (input:string) =
    input.Split('\n')
    |> Array.map (fun line -> 
        line.Split(',')
        |> Array.map int)

let manhattanDistance (p1:int[]) (p2:int[]) =
    Array.map2 (fun a b -> abs(a - b)) p1 p2
    |> Array.sum

let findConstellations (points:int[][]) =
    let rec exploreConstellation (point:int[]) visited =
        Array.exists (fun p -> not (Set.contains p visited) && manhattanDistance point p <= 3) points
        |> function
        | false -> visited
        | true -> 
            let neighbors = Array.filter (fun p -> not (Set.contains p visited) && manhattanDistance point p <= 3) points
            Array.fold (fun acc neighbor -> exploreConstellation neighbor acc) (Set.add point visited) neighbors

    let mutable constellations = 0
    let mutable visitedPoints = Set.empty

    for point in points do
        if not (Set.contains point visitedPoints) then
            visitedPoints <- exploreConstellation point visitedPoints
            constellations <- constellations + 1

    constellations

let input = File.ReadAllText "input.txt"
let points = parseInput input
let result = findConstellations points
printfn "%d" result


module Day4

let input = System.IO.File.ReadAllLines "input.txt"

let parseInput (line:string) =
    let parts = line.Split(',')
    let range1 = parts.[0].Split('-') |> Array.map int
    let range2 = parts.[1].Split('-') |> Array.map int
    (range1.[0], range1.[1], range2.[0], range2.[1])

let contains (a1, a2, b1, b2) =
    (a1 <= b1 && a2 >= b2) || (b1 <= a1 && b2 >= a2)

let overlaps (a1, a2, b1, b2) =
    a1 <= b2 && b1 <= a2

let countFullyContainedPairs =
    input
    |> Array.map parseInput
    |> Array.filter contains
    |> Array.length

let countOverlappingPairs =
    input
    |> Array.map parseInput
    |> Array.filter overlaps
    |> Array.length

printfn "%d" countFullyContainedPairs
printfn "%d" countOverlappingPairs

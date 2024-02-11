
module Day4

let input = System.IO.File.ReadAllLines "input.txt"

let parseInput (line:string) =
    let parts = line.Split(',')
    let range1 = parts.[0].Split('-') |> Array.map int
    let range2 = parts.[1].Split('-') |> Array.map int
    (range1.[0], range1.[1], range2.[0], range2.[1])

let checkContainment (r1Start, r1End, r2Start, r2End) =
    (r1Start <= r2Start && r1End >= r2End) || (r2Start <= r1Start && r2End >= r1End)

let countFullyContainedPairs =
    input
    |> Array.map parseInput
    |> Array.filter checkContainment
    |> Array.length

printfn "%d" countFullyContainedPairs

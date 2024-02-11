
module Day10

let input = System.IO.File.ReadAllLines "input.txt" |> Array.map int |> Array.sort

let rec countDifferences (arr: int[]) (prev: int) (diff1: int) (diff3: int) (index: int) =
    if index = arr.Length then diff1 * (diff3 + 1)
    else
        let diff = arr.[index] - prev
        if diff = 1 then countDifferences arr arr.[index] (diff1 + 1) diff3 (index + 1)
        elif diff = 3 then countDifferences arr arr.[index] diff1 (diff3 + 1) (index + 1)
        else countDifferences arr arr.[index] diff1 diff3 (index + 1)

let result = countDifferences input 0 0 0 0
printfn "%d" result

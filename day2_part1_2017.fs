
module Day2

let input = System.IO.File.ReadAllLines "input.txt"

let parseRow (row:string) =
    row.Split(' ','\t')
    |> Array.map int

let calculateChecksum (rows:string[]) =
    rows
    |> Array.map parseRow
    |> Array.map (fun row -> Array.max row - Array.min row)
    |> Array.sum

let result = calculateChecksum input
printfn "%d" result

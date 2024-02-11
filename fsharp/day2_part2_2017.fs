
module Day2

let input = System.IO.File.ReadAllLines "input.txt"

let part1 =
    input
    |> Array.map (fun line -> line.Split('\t') |> Array.map int)
    |> Array.map (fun row -> Array.max row - Array.min row)
    |> Array.sum

let part2 =
    input
    |> Array.map (fun line -> line.Split('\t') |> Array.map int)
    |> Array.map (fun row ->
        row
        |> Array.collect (fun x ->
            row
            |> Array.filter (fun y -> x <> y && x % y = 0)
            |> Array.map (fun y -> x / y)
        )
        |> Array.sum
    )
    |> Array.sum

printfn "%d" part1
printfn "%d" part2

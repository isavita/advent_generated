
module Day13

let input = System.IO.File.ReadAllLines "input.txt"

let layers = input |> Array.map (fun line -> 
    let parts = line.Split(": ")
    (int parts.[0], int parts.[1])
)

let severity = 
    layers
    |> Array.filter (fun (depth, range) -> depth % ((range - 1) * 2) = 0)
    |> Array.sumBy (fun (depth, range) -> depth * range)

printfn "%d" severity

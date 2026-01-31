
open System
open System.IO

let components =
    File.ReadAllLines("input.txt")
    |> Array.map (fun line ->
        let parts = line.Split('/')
        (int parts.[0], int parts.[1]))

let mutable maxStrength = 0

let rec dfs port strength (used: bool[]) =
    if strength > maxStrength then maxStrength <- strength
    for i = 0 to components.Length - 1 do
        if not used.[i] then
            let a, b = components.[i]
            if a = port || b = port then
                used.[i] <- true
                let nextPort = if a = port then b else a
                dfs nextPort (strength + a + b) used
                used.[i] <- false

[<EntryPoint>]
let main argv =
    let used = Array.create components.Length false
    dfs 0 0 used
    printfn "%d" maxStrength
    0

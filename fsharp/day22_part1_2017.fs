
open System
open System.IO
open System.Collections.Generic

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines("input.txt")
    let height = lines.Length
    let width = if height = 0 then 0 else lines.[0].Length
    let startX = width / 2
    let startY = height / 2
    let infected = HashSet<(int * int)>()
    for y in 0 .. height - 1 do
        let line = lines.[y]
        for x in 0 .. line.Length - 1 do
            if line.[x] = '#' then infected.Add((x, y)) |> ignore
    let dx = [| 0; 1; 0; -1 |]
    let dy = [| -1; 0; 1; 0 |]
    let mutable dir = 0
    let mutable x = startX
    let mutable y = startY
    let mutable newInfections = 0
    for _ in 1 .. 10000 do
        let pos = (x, y)
        if infected.Contains(pos) then
            dir <- (dir + 1) &&& 3
            infected.Remove(pos) |> ignore
        else
            dir <- (dir + 3) &&& 3
            infected.Add(pos) |> ignore
            newInfections <- newInfections + 1
        x <- x + dx.[dir]
        y <- y + dy.[dir]
    printfn "%d" newInfections
    0

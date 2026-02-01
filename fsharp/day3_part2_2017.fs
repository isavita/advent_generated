
open System
open System.IO
open System.Collections.Generic

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText "input.txt" |> int

    // Part 1
    let mutable layer = 0
    let mutable size = 1
    while size * size < input do
        layer <- layer + 1
        size <- size + 2
    let cycle = size - 1
    let offset = input - (size - 2) * (size - 2)
    let offsetMod = offset % cycle
    let steps = layer + abs (offsetMod - layer)
    Console.WriteLine(steps)

    // Part 2
    let map = Dictionary<(int * int), int64>()
    map.Add((0, 0), 1L)
    let mutable x = 0
    let mutable y = 0
    let mutable dx = 0
    let mutable dy = -1
    let mutable valNum = 1L
    while valNum <= int64 input do
        if (x = y) || (x < 0 && x = -y) || (x > 0 && x = 1 - y) then
            let t = dx
            dx <- -dy
            dy <- t
        x <- x + dx
        y <- y + dy
        let mutable sum = 0L
        for i in -1 .. 1 do
            for j in -1 .. 1 do
                let key = (x + i, y + j)
                match map.TryGetValue key with
                | true, v -> sum <- sum + v
                | _ -> ()
        map.[(x, y)] <- sum
        valNum <- sum
    Console.WriteLine(valNum)
    0

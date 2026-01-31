
open System
open System.IO

[<EntryPoint>]
let main argv =
    let lines = File.ReadAllLines("input.txt")
    let h = lines.Length
    if h = 0 then printfn "0"; 0 else

    let w = lines.[0].Length
    let grid = Array2D.init h w (fun y x -> lines.[y].[x])

    let antennas =
        [|for y in 0..h-1 do
            for x in 0..w-1 do
                let c = grid.[y,x]
                if c <> '.' then yield c,(y,x) |]
        |> Array.groupBy fst
        |> Array.map (fun (k,xs) -> k,xs |> Array.map snd)

    let mutable antinodes = Array2D.create h w false
    let inline (%) a b = let r = a % b in if r < 0 then r + b else r

    for (_,coords) in antennas do
        if coords.Length > 1 then
            for i in 0..coords.Length-1 do
                for j in i+1..coords.Length-1 do
                    let (y1,x1),(y2,x2) = coords.[i],coords.[j]
                    let dy, dx = y2 - y1, x2 - x1
                    let g = let rec gcd a b = if b = 0 then abs a else gcd b (a % b) in gcd dy dx
                    let sy, sx = dy / g, dx / g

                    if sx = 0 then
                        for y in 0..h-1 do
                            if not antinodes.[y,x1] then
                                antinodes.[y,x1] <- true
                    else
                        for y in 0..h-1 do
                            let num = sy * x1 - sx * y1 + sx * y
                            if num % sy = 0 then
                                let x = num / sy
                                if x >= 0 && x < w then
                                    if not antinodes.[y,x] then
                                        antinodes.[y,x] <- true

    let mutable count = 0
    for y in 0..h-1 do
        for x in 0..w-1 do
            if antinodes.[y,x] then count <- count + 1
    printfn "%d" count
    0

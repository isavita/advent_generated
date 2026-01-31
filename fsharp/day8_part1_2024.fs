
open System
open System.IO

let [<Literal>] MAX = 1000

let inline hash y x = y * 1001 + x

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let h = lines.Length
    let w = lines.[0].Length

    let antennas = Array.init 256 (fun _ -> ResizeArray())
    for y in 0..h-1 do
        let line = lines.[y]
        for x in 0..w-1 do
            let c = int line.[x]
            if c <> int '.' then antennas.[c].Add(struct(y,x))

    let antinodes = System.Collections.Generic.HashSet<int>()
    for i in 0..255 do
        let coords = antennas.[i]
        let n = coords.Count
        for j in 0..n-1 do
            let struct(ay,ax) = coords.[j]
            for k in j+1..n-1 do
                let struct(by,bx) = coords.[k]
                let p1y = 2*ay - by
                let p1x = 2*ax - bx
                if p1y >= 0 && p1y < h && p1x >= 0 && p1x < w then
                    antinodes.Add(hash p1y p1x) |> ignore
                let p2y = 2*by - ay
                let p2x = 2*bx - ax
                if p2y >= 0 && p2y < h && p2x >= 0 && p2x < w then
                    antinodes.Add(hash p2y p2x) |> ignore
    printfn "%d" antinodes.Count
    0

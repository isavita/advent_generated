
open System
open System.IO
open System.Text.RegularExpressions

type Sensor = { X:int; Y:int; Dist:int }

let manhattan x1 y1 x2 y2 = abs (x1 - x2) + abs (y1 - y2)

let parse (line:string) =
    let m = Regex.Matches(line, "-?\d+")
    let sx = int m.[0].Value
    let sy = int m.[1].Value
    let bx = int m.[2].Value
    let by = int m.[3].Value
    { X = sx; Y = sy; Dist = manhattan sx sy bx by }

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let sensors = lines |> Array.map parse
    let maxCoord = 4000000
    let mutable answer = -1L
    for x = 0 to maxCoord do
        let mutable y = 0
        while y <= maxCoord do
            let mutable detected = false
            let mutable i = 0
            while i < sensors.Length && not detected do
                let s = sensors.[i]
                if manhattan s.X s.Y x y <= s.Dist then
                    detected <- true
                    let d = s.Dist - abs (s.X - x)
                    y <- s.Y + d + 1
                i <- i + 1
            if not detected then
                answer <- int64 x * 4000000L + int64 y
                printfn "%d" answer
                Environment.Exit 0
        ()
    printfn "%d" answer
    0

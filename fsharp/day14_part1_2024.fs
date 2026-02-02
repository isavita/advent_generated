
open System
open System.IO

[<EntryPoint>]
let main _ =
    // constants
    let WIDTH   = 101
    let HEIGHT  = 103
    let STEPS   = 100
    let MAX_ROBOTS = 1000

    // fast positive modulo
    let inline modPos (a:int) (m:int) =
        let r = a % m
        if r < 0 then r + m else r

    // quadrant counters
    let mutable q1 = 0L
    let mutable q2 = 0L
    let mutable q3 = 0L
    let mutable q4 = 0L

    // read and process up to MAX_ROBOTS lines
    let mutable count = 0
    for line in File.ReadLines("input.txt") do
        if count >= MAX_ROBOTS then () else
        let parts = line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
        // parts[0] = "p=<x,y>", parts[1] = "v=<vx,vy>"
        let p = parts.[0].Substring(2).Split(',')
        let v = parts.[1].Substring(2).Split(',')
        let x0 = Int32.Parse(p.[0])
        let y0 = Int32.Parse(p.[1])
        let vx = Int32.Parse(v.[0])
        let vy = Int32.Parse(v.[1])

        let x = modPos (x0 + STEPS * vx) WIDTH
        let y = modPos (y0 + STEPS * vy) HEIGHT

        if x <> 50 && y <> 51 then
            if x < 50 && y < 51 then q1 <- q1 + 1L
            elif x > 50 && y < 51 then q2 <- q2 + 1L
            elif x < 50 && y > 51 then q3 <- q3 + 1L
            elif x > 50 && y > 51 then q4 <- q4 + 1L
        count <- count + 1

    let result = q1 * q2 * q3 * q4
    printfn "%d" result
    0

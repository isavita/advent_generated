
open System
open System.IO

let getCoord (s: string) =
    let idx = if s.Contains "+" then s.IndexOf "+" else s.IndexOf "="
    Int32.Parse(s.Substring(idx + 1))

let solve ax ay bx by px py =
    let mutable best = Int32.MaxValue
    for a in 0 .. 100 do
        let x = ax * a
        let y = ay * a
        if x <= px && y <= py then
            for b in 0 .. 100 do
                let xx = x + bx * b
                let yy = y + by * b
                if xx = px && yy = py then
                    let cost = a * 3 + b
                    if cost < best then best <- cost
    if best = Int32.MaxValue then -1 else best

let main () =
    let lines = File.ReadAllLines "input.txt"
    let mutable solved = 0
    let mutable total = 0L
    let mutable ax, ay, bx, by, px, py = 0, 0, 0, 0, 0, 0
    let mutable hasA, hasB, hasP = false, false, false

    for line in lines do
        let line = line.Trim()
        if line <> "" then
            if line.StartsWith "Button A:" || line.StartsWith "A:" then
                let parts = line.Split ':' |> Seq.skip 1 |> Seq.head |> fun s -> s.Split ','
                ax <- getCoord parts.[0]
                ay <- getCoord parts.[1]
                hasA <- true
            elif line.StartsWith "Button B:" || line.StartsWith "B:" then
                let parts = line.Split ':' |> Seq.skip 1 |> Seq.head |> fun s -> s.Split ','
                bx <- getCoord parts.[0]
                by <- getCoord parts.[1]
                hasB <- true
            elif line.StartsWith "Prize:" || line.StartsWith "P:" then
                let parts = line.Split ':' |> Seq.skip 1 |> Seq.head |> fun s -> s.Split ','
                px <- getCoord parts.[0]
                py <- getCoord parts.[1]
                hasP <- true
        else
            if hasA && hasB && hasP then
                let cost = solve ax ay bx by px py
                if cost <> -1 then
                    solved <- solved + 1
                    total <- total + int64 cost
            hasA <- false
            hasB <- false
            hasP <- false

    if hasA && hasB && hasP then
        let cost = solve ax ay bx by px py
        if cost <> -1 then
            solved <- solved + 1
            total <- total + int64 cost

    printfn "%d %d" solved total

main ()

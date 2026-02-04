
open System
open System.IO
open System.Collections.Generic

[<EntryPoint>]
let main _ =
    let maxGridSize = 1000
    let initialSandX = 500

    let lines = File.ReadAllLines "input.txt"
    let occupied = HashSet<int * int>()
    let mutable floorLevel = 0

    let parsePoint (s:string) =
        let parts = s.Split ','
        int parts.[0], int parts.[1]

    for line in lines do
        let tokens = line.Split([|" -> "|], StringSplitOptions.None)
        let mutable cur = parsePoint tokens.[0]
        for i in 1 .. tokens.Length - 1 do
            let nxt = parsePoint tokens.[i]
            if fst cur = fst nxt then
                let minY = min (snd cur) (snd nxt)
                let maxY = max (snd cur) (snd nxt)
                for y in minY .. maxY do
                    occupied.Add (fst cur, y) |> ignore
                    if y > floorLevel then floorLevel <- y
            else
                let minX = min (fst cur) (fst nxt)
                let maxX = max (fst cur) (fst nxt)
                for x in minX .. maxX do
                    occupied.Add (x, snd cur) |> ignore
                    if snd cur > floorLevel then floorLevel <- snd cur
            cur <- nxt

    floorLevel <- floorLevel + 1

    let mutable sands = 0
    let mutable firstFloorTouch = 0

    while not (occupied.Contains (initialSandX, 0)) do
        let mutable x = initialSandX
        let mutable y = 0
        let mutable settled = false
        while not settled do
            let mutable moved = false
            let ny = y + 1
            if not (occupied.Contains (x, ny)) then
                y <- ny
                moved <- true
            elif x > 0 && not (occupied.Contains (x - 1, ny)) then
                x <- x - 1
                y <- ny
                moved <- true
            elif x + 1 < maxGridSize && not (occupied.Contains (x + 1, ny)) then
                x <- x + 1
                y <- ny
                moved <- true

            if moved then
                if y = floorLevel then
                    if firstFloorTouch = 0 then firstFloorTouch <- sands
                    occupied.Add (x, y) |> ignore
                    settled <- true
            else
                occupied.Add (x, y) |> ignore
                settled <- true

        sands <- sands + 1

    printfn "%d" firstFloorTouch
    0

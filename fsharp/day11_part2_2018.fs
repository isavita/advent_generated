
open System
open System.IO

[<EntryPoint>]
let main _ =
    let serial = File.ReadAllText("input.txt").Trim() |> int
    let gridSize = 300
    let power = Array2D.zeroCreate<int> gridSize gridSize
    for y = 0 to gridSize - 1 do
        for x = 0 to gridSize - 1 do
            let rack = x + 11
            let level = ((rack * (y + 1) + serial) * rack / 100) % 10 - 5
            power.[y, x] <- level
    let sat = Array2D.zeroCreate<int> gridSize gridSize
    for y = 0 to gridSize - 1 do
        for x = 0 to gridSize - 1 do
            let v = power.[y, x]
            let left = if x > 0 then sat.[y, x - 1] else 0
            let top = if y > 0 then sat.[y - 1, x] else 0
            let topLeft = if x > 0 && y > 0 then sat.[y - 1, x - 1] else 0
            sat.[y, x] <- v + left + top - topLeft
    let mutable maxPower = Int32.MinValue
    let mutable bestX = 0
    let mutable bestY = 0
    let mutable bestSize = 0
    for size = 1 to gridSize do
        let limit = gridSize - size
        for y = 0 to limit do
            let y2 = y + size - 1
            for x = 0 to limit do
                let x2 = x + size - 1
                let mutable total = sat.[y2, x2]
                if y > 0 then total <- total - sat.[y - 1, x2]
                if x > 0 then total <- total - sat.[y2, x - 1]
                if y > 0 && x > 0 then total <- total + sat.[y - 1, x - 1]
                if total > maxPower then
                    maxPower <- total
                    bestX <- x + 1
                    bestY <- y + 1
                    bestSize <- size
    printfn "%d,%d,%d" bestX bestY bestSize
    0

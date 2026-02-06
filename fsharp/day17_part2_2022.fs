
open System.Collections.Generic
open System.IO

[<EntryPoint>]
let main _ =
    let jet = File.ReadAllText("input.txt").Trim()
    let jetLen = jet.Length
    let shapes = [|
        [|(0L, 0L); (1L, 0L); (2L, 0L); (3L, 0L)|]
        [|(1L, 0L); (0L, 1L); (1L, 1L); (2L, 1L); (1L, 2L)|]
        [|(0L, 0L); (1L, 0L); (2L, 0L); (2L, 1L); (2L, 2L)|]
        [|(0L, 0L); (0L, 1L); (0L, 2L); (0L, 3L)|]
        [|(0L, 0L); (1L, 0L); (0L, 1L); (1L, 1L)|]
    |]
    let chamber = Dictionary<int64, int>()
    let isOccupied x y =
        let mutable m = 0
        if chamber.TryGetValue(y, &m) then (m &&& (1 <<< x)) <> 0 else false
    let mutable jetIdx = 0
    let mutable highestY = 0L
    let mutable addedHeight = 0L
    let mutable rockCount = 0L
    let totalRocks = 1000000000000L
    let cache = Dictionary<int * int * int64 list, int64 * int64>()
    while rockCount < totalRocks do
        let rockIdx = int (rockCount % 5L)
        let shape = shapes.[rockIdx]
        let mutable currX, currY = 2, highestY + 4L
        let mutable settled = false
        while not settled do
            let dir = jet.[jetIdx]
            jetIdx <- (jetIdx + 1) % jetLen
            let dx = if dir = '>' then 1 else -1
            let mutable canMoveX = true
            for i = 0 to shape.Length - 1 do
                let sx, sy = shape.[i]
                let nx = int (sx + int64 currX) + dx
                if nx < 0 || nx >= 7 || isOccupied nx (sy + currY) then canMoveX <- false
            if canMoveX then currX <- currX + dx
            let mutable canMoveY = true
            for i = 0 to shape.Length - 1 do
                let sx, sy = shape.[i]
                let ny = sy + currY - 1L
                if ny <= 0L || isOccupied (int (sx + int64 currX)) ny then canMoveY <- false
            if canMoveY then currY <- currY - 1L
            else
                settled <- true
                for i = 0 to shape.Length - 1 do
                    let sx, sy = shape.[i]
                    let absX, absY = int (sx + int64 currX), sy + currY
                    let mutable m = 0
                    chamber.[absY] <- (if chamber.TryGetValue(absY, &m) then m else 0) ||| (1 <<< absX)
                    if absY > highestY then highestY <- absY
                if addedHeight = 0L then
                    let profile = List.init 7 (fun x ->
                        let mutable cy = highestY
                        while cy > 0L && not (isOccupied x cy) do cy <- cy - 1L
                        highestY - cy)
                    let state = (rockIdx, jetIdx, profile)
                    match cache.TryGetValue(state) with
                    | true, (pCount, pHeight) ->
                        let cLen = rockCount - pCount
                        let cHeight = highestY - pHeight
                        let numC = (totalRocks - 1L - rockCount) / cLen
                        addedHeight <- numC * cHeight
                        rockCount <- rockCount + numC * cLen
                    | _ -> cache.[state] <- (rockCount, highestY)
        rockCount <- rockCount + 1L
    printfn "%d" (highestY + addedHeight)
    0


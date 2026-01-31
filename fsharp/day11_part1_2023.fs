
open System.IO

let readFile path =
    File.ReadAllLines path |> Array.map (fun s -> s.ToCharArray())

let buildGrid (input: char[][]) =
    let height, width = input.Length, input.[0].Length
    let data = Array.zeroCreate (width * height)
    for y in 0..height-1 do
        for x in 0..width-1 do
            if input.[y].[x] <> '.' then data.[y * width + x] <- byte input.[y].[x]
    width, height, data

let isEmptyRow width (data: byte[]) y =
    let rec loop x = x >= width || (data.[y * width + x] = 0uy && loop (x + 1))
    loop 0

let isEmptyCol width height (data: byte[]) x =
    let rec loop y = y >= height || (data.[y * width + x] = 0uy && loop (y + 1))
    loop 0

let calculateOffsets emptyIndexes (offsets: int[]) bound =
    for ei in emptyIndexes do
        for j in ei + 1 .. bound - 1 do
            offsets.[j] <- offsets.[j] + 1

let expandGrid factor (width, height, data: byte[]) =
    let emptyRows = [| for y in 0..height-1 do if isEmptyRow width data y then yield y |]
    let emptyCols = [| for x in 0..width-1 do if isEmptyCol width height data x then yield x |]
    let dXs = Array.zeroCreate width
    let dYs = Array.zeroCreate height
    calculateOffsets emptyCols dXs width
    calculateOffsets emptyRows dYs height
    let newW = width + emptyCols.Length * (factor - 1)
    let newH = height + emptyRows.Length * (factor - 1)
    let newData = Array.zeroCreate (newW * newH)
    for y in 0..height-1 do
        for x in 0..width-1 do
            if data.[y * width + x] <> 0uy then
                let nx = x + dXs.[x] * (factor - 1)
                let ny = y + dYs.[y] * (factor - 1)
                newData.[ny * newW + nx] <- data.[y * width + x]
    newW, newH, newData

let manhattan (x1,y1) (x2,y2) = abs (x2 - x1) + abs (y2 - y1)

let solve factor (input: char[][]) =
    let width, height, data = buildGrid input
    let w, h, grid = expandGrid factor (width, height, data)
    let mutable res = 0
    let mutable seen = []
    for y in 0..h-1 do
        for x in 0..w-1 do
            if grid.[y * w + x] <> 0uy then
                for (px,py) in seen do
                    res <- res + manhattan (x,y) (px,py)
                seen <- (x,y)::seen
    res

[<EntryPoint>]
let main _ =
    printfn "%d" (solve 2 (readFile "input.txt"))
    0


open System
open System.IO

let checkMAS (grid: char[][]) x y dx dy =
    let rows = grid.Length
    let cols = grid.[0].Length
    if x < 0 || y < 0 || x >= rows || y >= cols then false else
    let mutable f = true
    let mutable b = true
    let pat = "MAS".ToCharArray()
    for i = 0 to 2 do
        let nx = x + dx * i
        let ny = y + dy * i
        if nx < 0 || ny < 0 || nx >= rows || ny >= cols || grid.[nx].[ny] <> pat.[i] then f <- false
        if nx < 0 || ny < 0 || nx >= rows || ny >= cols || grid.[nx].[ny] <> pat.[2 - i] then b <- false
    f || b

let checkXMAS (grid: char[][]) x y =
    (checkMAS grid (x-1) (y-1) 1 1 && checkMAS grid (x-1) (y+1) 1 -1) ||
    (checkMAS grid (x+1) (y-1) -1 1 && checkMAS grid (x+1) (y+1) -1 -1)

let countXMASPatterns (grid: char[][]) =
    let rows = grid.Length
    let cols = grid.[0].Length
    if rows < 3 || cols < 3 then 0 else
    let mutable cnt = 0
    for i = 1 to rows-2 do
        for j = 1 to cols-2 do
            if grid.[i].[j] = 'A' && checkXMAS grid i j then cnt <- cnt + 1
    cnt

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines("input.txt")
    let grid = lines |> Array.map (fun s -> s.ToCharArray())
    printfn "%d" (countXMASPatterns grid)
    0

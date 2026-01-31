
open System
open System.IO

let inline isDigit (c:char) = c >= '0' && c <= '9'
let inline isSymbol (c:char) = not (isDigit c || c = '.')

let readLines (path:string) = File.ReadAllLines path |> Array.map (fun s -> s.ToCharArray())

let extractNumber (grid:char[][]) y x =
    let rec loop x acc =
        if x < grid.[y].Length && isDigit grid.[y].[x] then
            loop (x+1) (acc * 10 + int (grid.[y].[x] - '0'))
        else acc
    let startX = x
    let mutable cur = startX
    while cur < grid.[y].Length && isDigit grid.[y].[cur] do cur <- cur + 1
    let len = cur - startX
    (loop startX 0, len)

let hasAdjacentSymbol (grid:char[][]) y x len =
    let rows, cols = grid.Length, grid.[y].Length
    let inBounds y x = y >= 0 && y < rows && x >= 0 && x < cols
    let mutable found = false
    let check y x =
        if inBounds y x && isSymbol grid.[y].[x] then found <- true
    for dy in [-1;0;1] do
        for dx in -1..len do
            check (y+dy) (x+dx)
    found

let solve (grid:char[][]) =
    let mutable sum = 0
    for y in 0..grid.Length-1 do
        let mutable x = 0
        while x < grid.[y].Length do
            if isDigit grid.[y].[x] then
                let num, len = extractNumber grid y x
                if hasAdjacentSymbol grid y x len then sum <- sum + num
                x <- x + len
            else x <- x + 1
    sum

[<EntryPoint>]
let main _ =
    readLines "input.txt" |> solve |> printfn "%d"
    0

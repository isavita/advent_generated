
open System
open System.IO

let readAsteroids (lines: string[]) =
    [| for line in lines -> [| for c in line -> c = '#' |] |]

let countVisible (field: bool[][]) x y =
    if not field.[y].[x] then 0 else
    let h = field.Length
    let w = field.[0].Length
    let seen = System.Collections.Generic.HashSet<float>()
    for oy in 0..h-1 do
        for ox in 0..w-1 do
            if field.[oy].[ox] && not (ox=x && oy=y) then
                let dx, dy = float (ox - x), float (oy - y)
                let angle = atan2 dy dx
                seen.Add angle |> ignore
    seen.Count

let findBest (field: bool[][]) =
    let h = field.Length
    let w = field.[0].Length
    let mutable best = 0
    for y in 0..h-1 do
        for x in 0..w-1 do
            let c = countVisible field x y
            if c > best then best <- c
    best

[<EntryPoint>]
let main _ =
    let field = readAsteroids (File.ReadAllLines "input.txt")
    printfn "%d" (findBest field)
    0

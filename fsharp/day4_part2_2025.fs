
open System.IO

let grid = File.ReadAllLines("input.txt") |> Array.map (fun s -> s.ToCharArray())
let R = grid.Length
if R = 0 then printfn "Total rolls removed: 0" else
let C = grid.[0].Length
let dr = [|-1;-1;-1;0;0;1;1;1|]
let dc = [|-1;0;1;-1;1;-1;0;1|]

let mutable removed = 0
let mutable changed = true
while changed do
    changed <- false
    for r in 0..R-1 do
        for c in 0..C-1 do
            if grid.[r].[c] = '@' then
                let mutable cnt = 0
                for k in 0..7 do
                    let nr, nc = r + dr.[k], c + dc.[k]
                    if nr >= 0 && nr < R && nc >= 0 && nc < C && grid.[nr].[nc] = '@' then cnt <- cnt + 1
                if cnt < 4 then grid.[r].[c] <- '*'; changed <- true
    for r in 0..R-1 do
        for c in 0..C-1 do
            if grid.[r].[c] = '*' then grid.[r].[c] <- '.'; removed <- removed + 1
printfn "Total rolls removed: %d" removed

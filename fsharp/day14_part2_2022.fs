
open System
open System.IO

let MAX_R = 200
let MAX_C = 1000
let EXTRA_SPACE = 250
let START_COL = 500

let matrix = Array2D.create MAX_R MAX_C '.'

let drawLine (minC:int) (r1,c1,r2,c2) =
    let adjC1 = c1 - minC + EXTRA_SPACE
    let adjC2 = c2 - minC + EXTRA_SPACE
    if r1 = r2 then
        let a,b = if adjC1<adjC2 then adjC1,adjC2 else adjC2,adjC1
        for c in a..b do matrix.[r1,c] <- '#'
    else
        let a,b = if r1<r2 then r1,r2 else r2,r1
        for r in a..b do matrix.[r,adjC1] <- '#'

let dropSand originC =
    if matrix.[0,originC] = 'o' then false else
    let rec loop r c =
        if r = MAX_R-1 then false else
        if matrix.[r+1,c] = '.' then loop (r+1) c
        elif c>0 && matrix.[r+1,c-1] = '.' then loop (r+1) (c-1)
        elif c<MAX_C-1 && matrix.[r+1,c+1] = '.' then loop (r+1) (c+1)
        else
            matrix.[r,c] <- 'o'
            true
    loop 0 originC

let input = File.ReadAllLines "input.txt"

let coords =
    input
    |> Array.collect (fun s -> s.Split([|" -> "|],StringSplitOptions.RemoveEmptyEntries))
    |> Array.map (fun p ->
        let a = p.Split(',')
        int a.[1], int a.[0])

let minC = coords |> Array.map snd |> Array.min
let maxR = coords |> Array.map fst |> Array.max

input
|> Array.iter (fun line ->
    let pts =
        line.Split([|" -> "|],StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun p ->
            let a = p.Split(',')
            int a.[1], int a.[0])
    pts.[1..]
    |> Array.iteri (fun i (r,c) ->
        let (pr,pc) = pts.[i]
        drawLine minC (pr,pc,r,c)))

let floorR = maxR + 2
for c in 0..MAX_C-1 do matrix.[floorR,c] <- '#'

let originC = START_COL - minC + EXTRA_SPACE

let rec sim count =
    if dropSand originC then sim (count+1) else count

printfn "%d" (sim 0)

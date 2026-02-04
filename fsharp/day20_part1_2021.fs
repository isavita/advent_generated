
open System
open System.IO

let offsets = [| -1; 0; 1 |]

let calcIndex (i:int) (j:int) (img:char[][]) flip =
    let rows = img.Length
    let cols = img.[0].Length
    let mutable idx = 0
    for di in offsets do
        for dj in offsets do
            idx <- idx <<< 1
            let ni = i + di
            let nj = j + dj
            if ni >= 0 && ni < rows && nj >= 0 && nj < cols then
                if img.[ni].[nj] = '#' then idx <- idx ||| 1
            elif flip then idx <- idx ||| 1
    idx

let apply (img:char[][]) (algo:string) flip =
    let r = img.Length
    let c = img.[0].Length
    Array.init (r + 2) (fun i ->
        Array.init (c + 2) (fun j ->
            let idx = calcIndex (i-1) (j-1) img flip
            algo.[idx]))

let enhance (img:char[][]) (algo:string) times =
    let mutable cur = img
    let infiniteFlip = algo.[0] = '#'
    for step = 1 to times do
        cur <- apply cur algo (step % 2 = 0 && infiniteFlip)
    cur

let countLit (img:char[][]) =
    img |> Array.sumBy (fun row -> row |> Array.sumBy (fun ch -> if ch = '#' then 1 else 0))

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let algorithm = lines.[0]
    let image = lines.[2..] |> Array.map (fun s -> s.ToCharArray())
    let result = enhance image algorithm 2 |> countLit
    printfn "%d" result
    0

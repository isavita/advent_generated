
open System.IO

let lines = File.ReadAllLines("input.txt")
let h = lines.Length
let w = lines.[0].Length
let g = Array2D.init h w (fun y x -> int lines.[y].[x] - int '0')

let dirs = [| (0,1);(0,-1);(1,0);(-1,0) |]

let rec walk y x (dy,dx) v d =
    let ny,nx = y+dy,x+dx
    if nx<0 || ny<0 || nx>=w || ny>=h then d
    else if g.[ny,nx]>=v then d+1
    else walk ny nx (dy,dx) v (d+1)

let score y x =
    let v = g.[y,x]
    dirs |> Array.fold (fun acc (dy,dx) -> acc * walk y x (dy,dx) v 0) 1

[0..h-1] |> List.collect (fun y -> [0..w-1] |> List.map (score y)) |> List.max |> printfn "%d"

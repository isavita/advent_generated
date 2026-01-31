
open System
open System.IO

let dirs = [| (0,-1); (-1,0); (0,1); (1,0) |]

let inline absInt x = if x < 0 then -x else x

let parseDir = function
    | 'U' -> dirs.[0]
    | 'L' -> dirs.[1]
    | 'D' -> dirs.[2]
    | 'R' -> dirs.[3]
    | _   -> (0,0)

let shoelace (poly: (int*int)[]) =
    let mutable area = 0
    let n = poly.Length
    for i in 0..n-1 do
        let j = (i+1)%n
        let (xi,yi) = poly.[i]
        let (xj,yj) = poly.[j]
        area <- area + xi*yj - yi*xj
    absInt area / 2

let perimeter (poly: (int*int)[]) =
    let mutable perim = 0
    let n = poly.Length
    for i in 0..n-1 do
        let j = (i+1)%n
        let (xi,yi) = poly.[i]
        let (xj,yj) = poly.[j]
        perim <- perim + absInt (xi - xj) + absInt (yi - yj)
    perim

let calcArea poly =
    shoelace poly + perimeter poly / 2 + 1

[<EntryPoint>]
let main _ =
    let poly = ResizeArray<int*int>()
    poly.Add(0,0)
    File.ReadAllLines("input.txt")
    |> Array.iter (fun line ->
        let dir = parseDir line.[0]
        let len = int (line.Split(' ').[1])
        let (x,y) = poly.[poly.Count-1]
        let (dx,dy) = dir
        poly.Add(x + dx*len, y + dy*len)
    )
    calcArea (poly.ToArray())
    |> printfn "%d"
    0

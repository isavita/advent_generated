
open System.IO

let SIZE = 71
let BYTES = 1024

let grid = Array2D.create SIZE SIZE false
let visited = Array2D.create SIZE SIZE false
let dx = [|1;-1;0;0|]
let dy = [|0;0;1;-1|]

File.ReadAllLines("input.txt")
|> Array.truncate BYTES
|> Array.iter (fun l ->
    let xy = l.Split(',')
    let x,y = int xy.[0], int xy.[1]
    if x>=0 && x<SIZE && y>=0 && y<SIZE then grid.[y,x] <- true)

let q = System.Collections.Generic.Queue<int*int*int>()
q.Enqueue(0,0,0)
visited.[0,0] <- true

let rec bfs () =
    if q.Count = 0 then
        printfn "No path"
    else
        let x,y,steps = q.Dequeue()
        if x = SIZE-1 && y = SIZE-1 then
            printfn "%d" steps
        else
            [0..3]
            |> List.iter (fun i ->
                let nx,ny = x+dx.[i], y+dy.[i]
                if nx>=0 && ny>=0 && nx<SIZE && ny<SIZE && not grid.[ny,nx] && not visited.[ny,nx] then
                    visited.[ny,nx] <- true
                    q.Enqueue(nx,ny,steps+1))
            bfs ()

bfs ()


open System
open System.IO

let SIZE = 71
let dirs = [| (1,0); (-1,0); (0,1); (0,-1) |]

let canReach (grid: bool[,]) =
    if grid.[0,0] || grid.[SIZE-1,SIZE-1] then false else
    let visited = Array2D.create SIZE SIZE false
    let q = System.Collections.Generic.Queue<int*int>()
    q.Enqueue((0,0))
    visited.[0,0] <- true
    let rec bfs () =
        if q.Count = 0 then false else
        let x,y = q.Dequeue()
        if x = SIZE-1 && y = SIZE-1 then true else
        dirs |> Array.iter (fun (dx,dy) ->
            let nx,ny = x+dx,y+dy
            if nx>=0 && ny>=0 && nx<SIZE && ny<SIZE && not grid.[ny,nx] && not visited.[ny,nx] then
                visited.[ny,nx] <- true
                q.Enqueue((nx,ny)))
        bfs ()
    bfs ()

[<EntryPoint>]
let main _ =
    let grid = Array2D.create SIZE SIZE false
    File.ReadAllLines "input.txt"
    |> Array.choose (fun s -> match s.Split(',') with [|x;y|] -> Some(int x,int y) | _ -> None)
    |> Array.iter (fun (x,y) ->
        grid.[y,x] <- true
        if not (canReach grid) then
            printfn "%d,%d" x y
            exit 0)
    printfn "No cutoff found"
    0

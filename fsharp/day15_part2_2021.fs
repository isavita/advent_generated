
open System
open System.IO
open System.Collections.Generic

let readGrid (path:string) =
    File.ReadAllLines(path)
    |> Array.map (fun line -> line.ToCharArray() |> Array.map (fun c -> int c - int '0'))

let dijkstra (grid:int[][]) =
    let rows = grid.Length
    let cols = grid.[0].Length
    let dist = Array2D.create rows cols Int32.MaxValue
    dist.[0,0] <- 0
    let pq = PriorityQueue<int*int*int,int>()
    pq.Enqueue((0,0,0),0)
    let dirs = [|(-1,0);(1,0);(0,-1);(0,1)|]
    let mutable result = -1
    while pq.Count > 0 && result = -1 do
        let (r,c,cost) = pq.Dequeue()
        if r = rows-1 && c = cols-1 then result <- cost
        elif cost <= dist.[r,c] then
            for (dr,dc) in dirs do
                let nr = r+dr
                let nc = c+dc
                if nr >= 0 && nr < rows && nc >= 0 && nc < cols then
                    let ncost = cost + grid.[nr].[nc]
                    if ncost < dist.[nr,nc] then
                        dist.[nr,nc] <- ncost
                        pq.Enqueue((nr,nc,ncost), ncost)
    result

let expand (grid:int[][]) factor =
    let orows = grid.Length
    let ocols = grid.[0].Length
    let nrows = orows * factor
    let ncols = ocols * factor
    Array.init nrows (fun i ->
        Array.init ncols (fun j ->
            let v = grid.[i % orows].[j % ocols]
            let inc = i / orows + j / ocols
            ((v + inc - 1) % 9) + 1))

[<EntryPoint>]
let main _ =
    let grid = readGrid "input.txt"
    printfn "Lowest total risk (Part 1): %d" (dijkstra grid)
    let big = expand grid 5
    printfn "Lowest total risk (Part 2): %d" (dijkstra big)
    0

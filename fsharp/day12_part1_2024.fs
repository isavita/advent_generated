
open System.IO

let dirs = [|-1,0;1,0;0,-1;0,1|]

let calcRegion (grid:string[]) (vis:bool[,]) r c =
    let rows,cols = Array.length grid,grid.[0].Length
    let ch = grid.[r].[c]
    let mutable area,perim,q = 0,0,System.Collections.Generic.Queue<int*int>()
    q.Enqueue((r,c)); vis.[r,c] <- true
    while q.Count > 0 do
        let x,y = q.Dequeue()
        area <- area + 1
        for dx,dy in dirs do
            let nx,ny = x+dx,y+dy
            if nx<0 || nx>=rows || ny<0 || ny>=cols || grid.[nx].[ny]<>ch then
                perim <- perim + 1
            elif not vis.[nx,ny] then
                vis.[nx,ny] <- true
                q.Enqueue((nx,ny))
    area*perim

[<EntryPoint>]
let main _ =
    let grid = File.ReadAllLines "input.txt"
    let rows,cols = Array.length grid,grid.[0].Length
    let vis = Array2D.create rows cols false
    let mutable total = 0
    for r in 0..rows-1 do
        for c in 0..cols-1 do
            if not vis.[r,c] then
                total <- total + calcRegion grid vis r c
    printfn "%d" total
    0

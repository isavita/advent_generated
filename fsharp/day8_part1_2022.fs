
open System.IO

let grid = File.ReadAllLines "input.txt" |> Array.map (fun s -> s.ToCharArray() |> Array.map (fun c -> int c - int '0'))
let n = grid.Length
let vis = Array2D.create n n 0

for i in 0..n-1 do
    for j in 0..n-1 do
        for dx,dy in [|0,1;0,-1;1,0;-1,0|] do
            let mutable x,y=i+dx,j+dy
            let mutable ok = true
            while ok && x>=0 && x<n && y>=0 && y<n do
                if grid.[x].[y]>=grid.[i].[j] then ok<-false
                x<-x+dx; y<-y+dy
            if ok then vis.[i,j]<-1

printfn "%d" (vis|>Seq.cast<int>|>Seq.sum)

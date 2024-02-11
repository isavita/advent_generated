
module Day18

open System.IO

let input = File.ReadAllLines "input.txt"

let sidesNotConnected (x,y,z) =
    let adjacent = [(x-1,y,z); (x+1,y,z); (x,y-1,z); (x,y+1,z); (x,y,z-1); (x,y,z+1)]
    let count = List.sumBy (fun (a,b,c) -> if input |> Array.contains (sprintf "%d,%d,%d" a b c) then 0 else 1) adjacent
    count

let totalSurfaceArea = 
    input 
    |> Array.map (fun line -> line.Split(',') |> Array.map int) 
    |> Array.sumBy (fun arr -> sidesNotConnected (arr.[0], arr.[1], arr.[2]))

printfn "%d" totalSurfaceArea

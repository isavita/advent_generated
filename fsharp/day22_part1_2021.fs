
open System
open System.IO

[<EntryPoint>]
let main _ =
    let grid = Array3D.create 101 101 101 false
    let clamp (v:int) = max -50 (min 50 v)

    File.ReadAllLines "input.txt"
    |> Array.iter (fun (s:string) ->
        let p = s.Split(' ')
        let on = p.[0] = "on"
        let c = p.[1].Split(',')
        let x0,x1 = let a=c.[0].[2..].Split("..") in (int a.[0],int a.[1])
        let y0,y1 = let a=c.[1].[2..].Split("..") in (int a.[0],int a.[1])
        let z0,z1 = let a=c.[2].[2..].Split("..") in (int a.[0],int a.[1])
        if x0 <= 50 && x1 >= -50 && y0 <= 50 && y1 >= -50 && z0 <= 50 && z1 >= -50 then
            for x in clamp x0 .. clamp x1 do
             for y in clamp y0 .. clamp y1 do
              for z in clamp z0 .. clamp z1 do
               Array3D.set grid (x+50) (y+50) (z+50) on)
    let mutable c = 0
    for x in 0..100 do
     for y in 0..100 do
      for z in 0..100 do
       if Array3D.get grid x y z then c <- c + 1
    printfn "%d" c
    0

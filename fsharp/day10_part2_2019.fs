
open System
open System.IO

type Asteroid = { X:int; Y:int; Angle:float; Dist:float }

let readAsteroids path =
    File.ReadAllLines path
    |> Array.mapi (fun y line -> line.ToCharArray() |> Array.mapi (fun x c -> ((x,y),c='#')) )
    |> Array.collect id |> Array.filter snd |> Array.map fst

let bestStation asteroids =
    asteroids
    |> Array.maxBy (fun (x,y) ->
        asteroids
        |> Array.filter (fun (a,b) -> (a,b)<>(x,y))
        |> Array.map (fun (a,b) -> atan2 (float (b-y)) (float (a-x)))
        |> Set.ofArray |> Set.count )

let vaporizeOrder asteroids (sx,sy) =
    asteroids
    |> Array.filter (fun (x,y) -> (x,y)<>(sx,sy))
    |> Array.map (fun (x,y) ->
        let dx,dy = float (x-sx), float (y-sy)
        let a = atan2 dy dx
        let a = if a < -Math.PI/2.0 then a + 2.0*Math.PI else a
        { X=x; Y=y; Angle=a; Dist=dx*dx+dy*dy } )
    |> Array.groupBy (fun a -> a.Angle)
    |> Array.map (fun (_,g) -> g |> Array.sortBy (fun a -> a.Dist))
    |> Array.sortBy (fun g -> g.[0].Angle)
    |> Array.unfold (fun groups ->
        if Array.isEmpty groups then None else
        let now = groups |> Array.map (fun g -> g.[0])
        let rest = groups |> Array.filter (fun g -> g.Length > 1) |> Array.map (fun g -> g.[1..])
        Some (now, rest) )
    |> Array.collect id

[<EntryPoint>]
let main _ =
    let asteroids = readAsteroids "input.txt"
    let station = bestStation asteroids
    let order = vaporizeOrder asteroids station
    if order.Length >= 200 then
        printfn "%d" (order.[199].X * 100 + order.[199].Y)
    0

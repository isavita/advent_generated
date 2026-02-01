
open System
open System.IO
open System.Collections.Generic

type Brick =
    { mutable X1:int; mutable Y1:int; mutable Z1:int
      mutable X2:int; mutable Y2:int; mutable Z2:int
      mutable BasedOn:ResizeArray<int>
      mutable Support:ResizeArray<int> }

let overlap a b =
    max a.X1 b.X1 <= min a.X2 b.X2 &&
    max a.Y1 b.Y1 <= min a.Y2 b.Y2

[<EntryPoint>]
let main _ =
    let bricks =
        File.ReadAllLines "input.txt"
        |> Array.map (fun line ->
            let p = line.Split '~'
            let c1 = p.[0].Split ',' |> Array.map int
            let c2 = p.[1].Split ',' |> Array.map int
            { X1 = c1.[0]; Y1 = c1.[1]; Z1 = c1.[2]
              X2 = c2.[0]; Y2 = c2.[1]; Z2 = c2.[2]
              BasedOn = ResizeArray()
              Support = ResizeArray() })
        |> Array.sortBy (fun b -> b.Z2)

    for i = 0 to bricks.Length-1 do
        let mutable bestZ = 0
        let supports = ResizeArray()
        for j = 0 to i-1 do
            if overlap bricks.[i] bricks.[j] then
                if bricks.[j].Z2 = bestZ then supports.Add j
                elif bricks.[j].Z2 > bestZ then
                    bestZ <- bricks.[j].Z2
                    supports.Clear()
                    supports.Add j
        let delta = bricks.[i].Z2 - bricks.[i].Z1
        bricks.[i].Z1 <- bestZ + 1
        bricks.[i].Z2 <- bricks.[i].Z1 + delta
        for b in supports do
            bricks.[i].BasedOn.Add b
            bricks.[b].Support.Add i

    let mutable total = 0
    let q = Queue<int>()
    let falling = HashSet<int>()
    for i = 0 to bricks.Length-1 do
        q.Clear()
        falling.Clear()
        for b in bricks.[i].Support do
            if bricks.[b].BasedOn.Count = 1 then q.Enqueue b
        while q.Count > 0 do
            let b = q.Dequeue()
            if falling.Add b then
                for s in bricks.[b].Support do
                    if bricks.[s].BasedOn |> Seq.forall falling.Contains then
                        q.Enqueue s
        total <- total + falling.Count
    printfn "%d" total
    0

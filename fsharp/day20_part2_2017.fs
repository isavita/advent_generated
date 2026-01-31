
open System
open System.Collections.Generic
open System.IO

type Particle = {
    mutable p : int * int * int
    mutable v : int * int * int
    a : int * int * int
    mutable alive : bool
}

[<EntryPoint>]
let main _ =
    let particles =
        File.ReadAllLines("input.txt")
        |> Array.map (fun line ->
            let parts = line.Split([|','; '<'; '>'; '='; ' '; '\t'|], StringSplitOptions.RemoveEmptyEntries)
            let p = (int parts.[1], int parts.[2], int parts.[3])
            let v = (int parts.[5], int parts.[6], int parts.[7])
            let a = (int parts.[9], int parts.[10], int parts.[11])
            { p = p; v = v; a = a; alive = true })
    for _ in 1 .. 1000 do
        for p in particles do
            if p.alive then
                let (vx,vy,vz) = p.v
                let (ax,ay,az) = p.a
                let v' = (vx+ax, vy+ay, vz+az)
                p.v <- v'
                let (px,py,pz) = p.p
                let (dx,dy,dz) = v'
                p.p <- (px+dx, py+dy, pz+dz)
        let dict = Dictionary<string, List<int>>()
        for i in 0 .. particles.Length-1 do
            let p = particles.[i]
            if p.alive then
                let (x,y,z) = p.p
                let key = sprintf "%d,%d,%d" x y z
                match dict.TryGetValue(key) with
                | true, lst -> lst.Add(i)
                | _ -> dict.[key] <- List([i])
        for kvp in dict do
            if kvp.Value.Count > 1 then
                for idx in kvp.Value do
                    particles.[idx].alive <- false
    let remaining = particles |> Array.filter (fun p -> p.alive) |> Array.length
    printfn "%d" remaining
    0

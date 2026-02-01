
open System
open System.IO

type Vec3 = { mutable X:int; mutable Y:int; mutable Z:int }
type Moon = { mutable Pos:Vec3; mutable Vel:Vec3 }

let gravity (moons:Moon[]) axis =
    for i in 0..moons.Length-1 do
        for j in i+1..moons.Length-1 do
            match axis with
            | 'x' when moons.[i].Pos.X > moons.[j].Pos.X ->
                moons.[i].Vel.X <- moons.[i].Vel.X - 1
                moons.[j].Vel.X <- moons.[j].Vel.X + 1
            | 'x' when moons.[i].Pos.X < moons.[j].Pos.X ->
                moons.[i].Vel.X <- moons.[i].Vel.X + 1
                moons.[j].Vel.X <- moons.[j].Vel.X - 1
            | 'y' when moons.[i].Pos.Y > moons.[j].Pos.Y ->
                moons.[i].Vel.Y <- moons.[i].Vel.Y - 1
                moons.[j].Vel.Y <- moons.[j].Vel.Y + 1
            | 'y' when moons.[i].Pos.Y < moons.[j].Pos.Y ->
                moons.[i].Vel.Y <- moons.[i].Vel.Y + 1
                moons.[j].Vel.Y <- moons.[j].Vel.Y - 1
            | 'z' when moons.[i].Pos.Z > moons.[j].Pos.Z ->
                moons.[i].Vel.Z <- moons.[i].Vel.Z - 1
                moons.[j].Vel.Z <- moons.[j].Vel.Z + 1
            | 'z' when moons.[i].Pos.Z < moons.[j].Pos.Z ->
                moons.[i].Vel.Z <- moons.[i].Vel.Z + 1
                moons.[j].Vel.Z <- moons.[j].Vel.Z - 1
            | _ -> ()

let velocity (moons:Moon[]) axis =
    for i in 0..moons.Length-1 do
        match axis with
        | 'x' -> moons.[i].Pos.X <- moons.[i].Pos.X + moons.[i].Vel.X
        | 'y' -> moons.[i].Pos.Y <- moons.[i].Pos.Y + moons.[i].Vel.Y
        | 'z' -> moons.[i].Pos.Z <- moons.[i].Pos.Z + moons.[i].Vel.Z
        | _ -> ()

let gcd a b =
    let rec loop a b = if b = 0L then a else loop b (a % b)
    loop (abs a) (abs b)

let lcm a b = abs (a / gcd a b * b)

let cycle (initial:Moon[]) axis =
    let moons = Array.map (fun m -> { Pos={X=m.Pos.X;Y=m.Pos.Y;Z=m.Pos.Z}; Vel={X=m.Vel.X;Y=m.Vel.Y;Z=m.Vel.Z} }) initial
    let rec loop steps =
        gravity moons axis
        velocity moons axis
        let ok = Array.forall2 (fun a b ->
            match axis with
            | 'x' -> a.Pos.X = b.Pos.X && a.Vel.X = b.Vel.X
            | 'y' -> a.Pos.Y = b.Pos.Y && a.Vel.Y = b.Vel.Y
            | 'z' -> a.Pos.Z = b.Pos.Z && a.Vel.Z = b.Vel.Z
            | _ -> false) moons initial
        if ok then steps else loop (steps + 1L)
    loop 1L

[<EntryPoint>]
let main _ =
    let parse (s:string) =
        let p = s.Trim('<','>','\n').Split(',') |> Array.map (fun t -> t.Trim().Split('=').[1] |> int)
        { Pos={X=p.[0];Y=p.[1];Z=p.[2]}; Vel={X=0;Y=0;Z=0} }
    let moons = File.ReadAllLines("input.txt") |> Array.map parse
    let cx = cycle moons 'x'
    let cy = cycle moons 'y'
    let cz = cycle moons 'z'
    printfn "%d" (lcm (lcm cx cy) cz)
    0


open System
open System.IO

type Point = { Px:double; Py:double; Pz:double; Vx:double; Vy:double; Vz:double }

let parse (s:string) =
    let p = s.Split([|'@'|], StringSplitOptions.RemoveEmptyEntries)
    let pos = p.[0].Split([|','|], StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun x -> Double.Parse(x.Trim()))
    let vel = p.[1].Split([|','|], StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun x -> Double.Parse(x.Trim()))
    { Px=pos.[0]; Py=pos.[1]; Pz=pos.[2]; Vx=vel.[0]; Vy=vel.[1]; Vz=vel.[2] }

let intersects p1 p2 =
    let det = p1.Vx * p2.Vy - p2.Vx * p1.Vy
    if abs det < 1e-12 then None else
    let dt = (p2.Px - p1.Px, p2.Py - p1.Py)
    let t1 = (p2.Vy * fst dt - p2.Vx * snd dt) / det
    let t2 = (p1.Vy * fst dt - p1.Vx * snd dt) / det
    if t1 < 0.0 || t2 < 0.0 then None else
    let x = p1.Px + p1.Vx * t1
    let y = p1.Py + p1.Vy * t1
    Some (x, y)

[<EntryPoint>]
let main _ =
    let pts = File.ReadAllLines("input.txt") |> Array.map parse
    let lo, hi = 200000000000000.0, 400000000000000.0
    let ok p =
        let x, y = p
        x >= lo && x <= hi && y >= lo && y <= hi
    seq { for i in 0..pts.Length-1 do
          for j in 0..i-1 do
          match intersects pts.[i] pts.[j] with
          | Some p when ok p -> yield () | _ -> () }
    |> Seq.length |> printfn "%d"
    0

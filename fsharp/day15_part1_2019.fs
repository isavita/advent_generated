
open System
open System.IO
open System.Collections.Generic

type State = { Mem: Map<int64, int64>; Ip: int64; Rb: int64; Halted: bool }

let getVal m v s =
    match m with
    | 0 -> Map.tryFind v s.Mem |> Option.defaultValue 0L
    | 1 -> v
    | 2 -> Map.tryFind (s.Rb + v) s.Mem |> Option.defaultValue 0L
    | _ -> 0L

let getAddr m v s = if m = 2 then s.Rb + v else v

let step input state =
    let mutable s = state
    let mutable out = None
    while not s.Halted && out.IsNone do
        let ins = Map.tryFind s.Ip s.Mem |> Option.defaultValue 0L
        let op, m1, m2, m3 = int (ins % 100L), int (ins / 100L % 10L), int (ins / 1000L % 10L), int (ins / 10000L % 10L)
        let pv i = Map.tryFind (s.Ip + int64 i) s.Mem |> Option.defaultValue 0L
        let g m v = getVal m v s
        let a m v = getAddr m v s
        match op with
        | 1 -> s <- { s with Mem = s.Mem.Add(a m3 (pv 3), g m1 (pv 1) + g m2 (pv 2)); Ip = s.Ip + 4L }
        | 2 -> s <- { s with Mem = s.Mem.Add(a m3 (pv 3), g m1 (pv 1) * g m2 (pv 2)); Ip = s.Ip + 4L }
        | 3 -> s <- { s with Mem = s.Mem.Add(a m1 (pv 1), input); Ip = s.Ip + 2L }
        | 4 -> out <- Some (g m1 (pv 1)); s <- { s with Ip = s.Ip + 2L }
        | 5 -> s <- { s with Ip = if g m1 (pv 1) <> 0L then g m2 (pv 2) else s.Ip + 3L }
        | 6 -> s <- { s with Ip = if g m1 (pv 1) = 0L then g m2 (pv 2) else s.Ip + 3L }
        | 7 -> s <- { s with Mem = s.Mem.Add(a m3 (pv 3), if g m1 (pv 1) < g m2 (pv 2) then 1L else 0L); Ip = s.Ip + 4L }
        | 8 -> s <- { s with Mem = s.Mem.Add(a m3 (pv 3), if g m1 (pv 1) = g m2 (pv 2) then 1L else 0L); Ip = s.Ip + 4L }
        | 9 -> s <- { s with Rb = s.Rb + g m1 (pv 1); Ip = s.Ip + 2L }
        | 99 -> s <- { s with Halted = true }
        | _ -> s <- { s with Halted = true }
    s, out

[<EntryPoint>]
let main _ =
    let code = 
        File.ReadAllText("input.txt").Split([|','|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.mapi (fun i v -> int64 i, int64 (v.Trim())) 
        |> Map.ofArray
    let s = { Mem = code; Ip = 0L; Rb = 0L; Halted = false }
    let q = Queue<int * int * int * State>()
    let v = HashSet<int * int>()
    q.Enqueue(0, 0, 0, s)
    v.Add((0, 0)) |> ignore
    let mutable found = false
    while q.Count > 0 && not found do
        let (x, y, d, currS) = q.Dequeue()
        for i in 1..4 do
            let nx, ny = match i with 1 -> x, y + 1 | 2 -> x, y - 1 | 3 -> x - 1, y | 4 -> x + 1, y | _ -> x, y
            if not (v.Contains((nx, ny))) then
                let nextS, out = step (int64 i) currS
                match out with
                | Some 0L -> v.Add((nx, ny)) |> ignore
                | Some 1L -> v.Add((nx, ny)) |> ignore; q.Enqueue(nx, ny, d+1, nextS)
                | Some 2L -> printfn "%d" (d+1); found <- true
                | _ -> ()
    0

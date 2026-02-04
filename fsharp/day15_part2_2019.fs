
open System
open System.IO
open System.Collections.Generic
open System.Linq

type IntCode(program: int64[]) =
    let data = Dictionary<int64, int64>()
    do for i = 0 to program.Length - 1 do data.[int64 i] <- program.[i]
    let mutable ip = 0L
    let mutable rel = 0L
    let get (i:int64) (mode:int) =
        let v = if data.ContainsKey(ip + i) then data.[ip + i] else 0L
        match mode with
        | 0 -> if data.ContainsKey(v) then data.[v] else 0L
        | 1 -> v
        | _ -> if data.ContainsKey(rel + v) then data.[rel + v] else 0L
    let set (i:int64) (mode:int) (value:int64) =
        let target = if data.ContainsKey(ip + i) then data.[ip + i] else 0L
        if mode = 0 then data.[target] <- value
        else data.[rel + target] <- value
    member _.Run(input:int) : int64 =
        let mutable out = 0L
        let mutable haveOut = false
        while not haveOut do
            let op = data.[ip] % 100L
            let m1 = int (data.[ip] / 100L % 10L)
            let m2 = int (data.[ip] / 1000L % 10L)
            let m3 = int (data.[ip] / 10000L % 10L)
            match op with
            | 1L -> set 3L m3 (get 1L m1 + get 2L m2); ip <- ip + 4L
            | 2L -> set 3L m3 (get 1L m1 * get 2L m2); ip <- ip + 4L
            | 3L -> set 1L m1 (int64 input); ip <- ip + 2L
            | 4L -> out <- get 1L m1; ip <- ip + 2L; haveOut <- true
            | 5L -> ip <- if get 1L m1 <> 0L then get 2L m2 else ip + 3L
            | 6L -> ip <- if get 1L m1 = 0L then get 2L m2 else ip + 3L
            | 7L -> set 3L m3 (if get 1L m1 < get 2L m2 then 1L else 0L); ip <- ip + 4L
            | 8L -> set 3L m3 (if get 1L m1 = get 2L m2 then 1L else 0L); ip <- ip + 4L
            | 9L -> rel <- rel + get 1L m1; ip <- ip + 2L
            | _  -> haveOut <- true
        out

let dx = [|0;0;0;-1;1|]
let dy = [|0;1;-1;0;0|]
let opp = [|0;2;1;4;3|]

let grid = Dictionary<(int*int), int>()
let mutable oxygen = (0,0)

let rec explore (robot:IntCode) x y =
    for d = 1 to 4 do
        let nx = x + dx.[d]
        let ny = y + dy.[d]
        if not (grid.ContainsKey((nx,ny))) then
            let status = int (robot.Run d)
            grid.[(nx,ny)] <- status
            if status <> 0 then
                if status = 2 then oxygen <- (nx,ny)
                explore robot nx ny
                robot.Run opp.[d] |> ignore

[<EntryPoint>]
let main _ =
    let code = File.ReadAllText("input.txt").Split(',') |> Array.map int64
    grid.[(0,0)] <- 1
    explore (IntCode code) 0 0
    let q = Queue<((int*int)*int)>()
    q.Enqueue((oxygen,0))
    let visited = HashSet<(int*int)>()
    visited.Add oxygen |> ignore
    let mutable maxDist = 0
    while q.Count > 0 do
        let ((px,py),d) = q.Dequeue()
        if d > maxDist then maxDist <- d
        for dir = 1 to 4 do
            let np = (px + dx.[dir], py + dy.[dir])
            if grid.ContainsKey np && grid.[np] <> 0 && visited.Add np then
                q.Enqueue((np,d+1))
    printfn "%d" maxDist
    0

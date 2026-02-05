
open System.Collections.Generic
open System.IO

type Computer(program: int64[]) =
    let mem = Dictionary<int64, int64>()
    do program |> Array.iteri (fun i v -> mem.[int64 i] <- v)
    let mutable ip, rb = 0L, 0L
    let inputs, outputs = Queue<int64>(), Queue<int64>()
    let mutable halted = false
    member _.Inputs = inputs
    member _.Outputs = outputs
    member _.Run() =
        let mutable blocked = false
        while not halted && not blocked do
            let get m = if mem.ContainsKey m then mem.[m] else 0L
            let ins = get ip
            let op = int (ins % 100L)
            let m1, m2, m3 = int (ins / 100L % 10L), int (ins / 1000L % 10L), int (ins / 10000L % 10L)
            let getAddr n =
                let v = get (ip + int64 n)
                match (if n=1 then m1 elif n=2 then m2 else m3) with 2 -> rb + v | _ -> v
            let getParam n =
                let v = get (ip + int64 n)
                match (if n=1 then m1 elif n=2 then m2 else m3) with 0 -> get v | 1 -> v | 2 -> get (rb + v) | _ -> 0L
            match op with
            | 1 -> mem.[getAddr 3] <- getParam 1 + getParam 2; ip <- ip + 4L
            | 2 -> mem.[getAddr 3] <- getParam 1 * getParam 2; ip <- ip + 4L
            | 3 -> if inputs.Count > 0 then mem.[getAddr 1] <- inputs.Dequeue(); ip <- ip + 2L else blocked <- true
            | 4 -> outputs.Enqueue(getParam 1); ip <- ip + 2L
            | 5 -> ip <- if getParam 1 <> 0L then getParam 2 else ip + 3L
            | 6 -> ip <- if getParam 1 = 0L then getParam 2 else ip + 3L
            | 7 -> mem.[getAddr 3] <- (if getParam 1 < getParam 2 then 1L else 0L); ip <- ip + 4L
            | 8 -> mem.[getAddr 3] <- (if getParam 1 = getParam 2 then 1L else 0L); ip <- ip + 4L
            | 9 -> rb <- rb + getParam 1; ip <- ip + 2L
            | 99 -> halted <- true
            | _ -> halted <- true

[<EntryPoint>]
let main _ =
    let prog = File.ReadAllText("input.txt").Split([|','; '\n'; '\r'|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int64
    let comps = Array.init 50 (fun i ->
        let c = Computer(prog)
        c.Inputs.Enqueue(int64 i)
        c)
    let pQueues = Array.init 50 (fun _ -> Queue<int64 * int64>())
    let mutable natX, natY, lastY, running = 0L, -1L, -2L, true
    while running do
        let mutable idle = true
        for i in 0 .. 49 do
            let c = comps.[i]
            if pQueues.[i].Count > 0 then
                idle <- false
                while pQueues.[i].Count > 0 do
                    let x, y = pQueues.[i].Dequeue()
                    c.Inputs.Enqueue x; c.Inputs.Enqueue y
            elif c.Inputs.Count = 0 then c.Inputs.Enqueue -1L
            else idle <- false
            c.Run()
            if c.Outputs.Count > 0 then
                idle <- false
                while c.Outputs.Count >= 3 do
                    let d, x, y = int (c.Outputs.Dequeue()), c.Outputs.Dequeue(), c.Outputs.Dequeue()
                    if d = 255 then natX <- x; natY <- y
                    else pQueues.[d].Enqueue(x, y)
        if idle && (pQueues |> Array.forall (fun q -> q.Count = 0)) && natY <> -1L then
            if natY = lastY then (printfn "%d" natY; running <- false)
            else (lastY <- natY; pQueues.[0].Enqueue(natX, natY))
    0

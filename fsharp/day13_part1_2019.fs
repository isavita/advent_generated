
open System
open System.IO
open System.Collections.Generic

type Machine(program: int64[]) =
    let mem = Dictionary<int64,int64>()
    do for i in 0L .. int64 program.Length - 1L do mem.[i] <- program.[int i]
    let mutable ip = 0L
    let mutable rel = 0L
    member _.Run(inp: Queue<int64>, outp: Queue<int64>) =
        let get a m =
            match m with
            | 0 -> let a = mem.[a] in if mem.ContainsKey a then mem.[a] else 0L
            | 1 -> if mem.ContainsKey a then mem.[a] else 0L
            | 2 -> let a = rel + mem.[a] in if mem.ContainsKey a then mem.[a] else 0L
            | _ -> failwith "mode"
        let set a m v =
            match m with
            | 0 -> mem.[mem.[a]] <- v
            | 2 -> mem.[rel + mem.[a]] <- v
            | _ -> failwith "mode"
        let mutable running = true
        while running do
            let op = mem.[ip]
            let modes = [| (op/100L)%10L; (op/1000L)%10L; (op/10000L)%10L |] |> Array.map int
            let code = op % 100L
            match code with
            | 1L ->
                let v = get (ip+1L) modes.[0] + get (ip+2L) modes.[1]
                set (ip+3L) modes.[2] v; ip <- ip + 4L
            | 2L ->
                let v = get (ip+1L) modes.[0] * get (ip+2L) modes.[1]
                set (ip+3L) modes.[2] v; ip <- ip + 4L
            | 3L ->
                if inp.Count = 0 then failwith "no input"
                set (ip+1L) modes.[0] (inp.Dequeue()); ip <- ip + 2L
            | 4L ->
                outp.Enqueue(get (ip+1L) modes.[0]); ip <- ip + 2L
            | 5L ->
                if get (ip+1L) modes.[0] <> 0L then ip <- get (ip+2L) modes.[1] else ip <- ip + 3L
            | 6L ->
                if get (ip+1L) modes.[0] = 0L then ip <- get (ip+2L) modes.[1] else ip <- ip + 3L
            | 7L ->
                set (ip+3L) modes.[2] (if get (ip+1L) modes.[0] < get (ip+2L) modes.[1] then 1L else 0L); ip <- ip + 4L
            | 8L ->
                set (ip+3L) modes.[2] (if get (ip+1L) modes.[0] = get (ip+2L) modes.[1] then 1L else 0L); ip <- ip + 4L
            | 9L ->
                rel <- rel + get (ip+1L) modes.[0]; ip <- ip + 2L
            | 99L -> running <- false
            | _ -> failwith "opcode"

let countBlocks (prog: int64[]) =
    let grid = Dictionary<string,int64>()
    let inp = Queue<int64>()
    let outp = Queue<int64>()
    let m = Machine(prog)
    m.Run(inp, outp)
    while outp.Count > 0 do
        let x = outp.Dequeue()
        let y = outp.Dequeue()
        let v = outp.Dequeue()
        grid.[sprintf "%d,%d" x y] <- v
    grid.Values |> Seq.filter ((=) 2L) |> Seq.length

[<EntryPoint>]
let main _ =
    let prog = File.ReadAllText("input.txt").Trim().Split(',') |> Array.map int64
    printfn "%d" (countBlocks prog)
    0

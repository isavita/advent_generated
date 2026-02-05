
open System.Collections.Generic
open System.IO

[<EntryPoint>]
let main _ =
    let inputStr = File.ReadAllText("input.txt").Trim()
    let program = inputStr.Split([|','|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int64
    let mem = Dictionary<int64, int64>()
    for i in 0 .. program.Length - 1 do mem.[int64 i] <- program.[i]
    
    let mutable ip = 0L
    let mutable halted = false
    let inputs = Queue<int64>()
    let outputs = List<int64>()
    
    let read addr = if mem.ContainsKey addr then mem.[addr] else 0L
    let write addr value = mem.[addr] <- value
    
    let run () =
        outputs.Clear()
        let mutable yielded = false
        while not halted && not yielded do
            let instr = read ip
            let opcode = instr % 100L
            let getAddr n =
                let mode = match n with 1 -> (instr / 100L) % 10L | 2 -> (instr / 1000L) % 10L | 3 -> (instr / 10000L) % 10L | _ -> 0L
                if mode = 1L then ip + int64 n else read (ip + int64 n)
            match opcode with
            | 1L | 2L | 7L | 8L ->
                let a1, a2, a3 = getAddr 1, getAddr 2, getAddr 3
                let v1, v2 = read a1, read a2
                let res = 
                    match opcode with
                    | 1L -> v1 + v2
                    | 2L -> v1 * v2
                    | 7L -> if v1 < v2 then 1L else 0L
                    | _ -> if v1 = v2 then 1L else 0L
                write a3 res
                ip <- ip + 4L
            | 3L ->
                if inputs.Count = 0 then yielded <- true
                else write (getAddr 1) (inputs.Dequeue()); ip <- ip + 2L
            | 4L ->
                outputs.Add(read (getAddr 1))
                ip <- ip + 2L
            | 5L | 6L ->
                let a1, a2 = getAddr 1, getAddr 2
                let v1, v2 = read a1, read a2
                if (opcode = 5L && v1 <> 0L) || (opcode = 6L && v1 = 0L) then ip <- v2 else ip <- ip + 3L
            | 99L -> halted <- true
            | _ -> halted <- true

    let grid = Dictionary<int * int, int64>()
    let mutable x, y, dir = 0, 0, 0
    let dx = [| 0; 1; 0; -1 |]
    let dy = [| -1; 0; 1; 0 |]
    
    while not halted do
        let color = if grid.ContainsKey(x, y) then grid.[(x, y)] else 0L
        inputs.Enqueue(color)
        run()
        if outputs.Count >= 2 then
            grid.[(x, y)] <- outputs.[0]
            dir <- if outputs.[1] = 0L then (dir + 3) % 4 else (dir + 1) % 4
            x <- x + dx.[dir]
            y <- y + dy.[dir]
    
    printfn "%d" grid.Count
    0


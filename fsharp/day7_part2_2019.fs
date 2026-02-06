
open System
open System.Collections.Generic
open System.IO

type Computer(program: int64[]) =
    let mem = Array.zeroCreate (max 10000 program.Length)
    do Array.blit program 0 mem 0 program.Length
    let mutable ip = 0
    let mutable rb = 0
    let mutable halted = false
    let mutable lastOut = 0L
    let inputs = Queue<int64>()
    member _.AddInput v = inputs.Enqueue v
    member _.IsHalted = halted
    member _.Run() =
        let mutable running = true
        while running do
            let instr = int mem.[ip]
            let opcode = instr % 100
            let getAddr n =
                let m = match n with 1 -> 100 | 2 -> 1000 | 3 -> 10000 | _ -> 1
                match (instr / m) % 10 with
                | 0 -> int mem.[ip + n]
                | 1 -> ip + n
                | 2 -> int mem.[ip + n] + rb
                | _ -> 0
            match opcode with
            | 1 -> mem.[getAddr 3] <- mem.[getAddr 1] + mem.[getAddr 2]; ip <- ip + 4
            | 2 -> mem.[getAddr 3] <- mem.[getAddr 1] * mem.[getAddr 2]; ip <- ip + 4
            | 3 -> if inputs.Count = 0 then running <- false else mem.[getAddr 1] <- inputs.Dequeue(); ip <- ip + 2
            | 4 -> lastOut <- mem.[getAddr 1]; ip <- ip + 2; running <- false
            | 5 -> if mem.[getAddr 1] <> 0L then ip <- int mem.[getAddr 2] else ip <- ip + 3
            | 6 -> if mem.[getAddr 1] = 0L then ip <- int mem.[getAddr 2] else ip <- ip + 3
            | 7 -> mem.[getAddr 3] <- (if mem.[getAddr 1] < mem.[getAddr 2] then 1L else 0L); ip <- ip + 4
            | 8 -> mem.[getAddr 3] <- (if mem.[getAddr 1] = mem.[getAddr 2] then 1L else 0L); ip <- ip + 4
            | 9 -> rb <- rb + int mem.[getAddr 1]; ip <- ip + 2
            | 99 -> halted <- true; running <- false
            | _ -> running <- false
        lastOut

let rec permute = function
    | [] -> [[]]
    | l -> [ for x in l do for p in permute (List.filter ((<>) x) l) -> x :: p ]

let runAmps input phases fb =
    let n = List.length phases
    let amps = Array.init n (fun _ -> Computer(input))
    let mutable signal, ampIdx, firstRound = 0L, 0, true
    while (if fb then not amps.[n-1].IsHalted else ampIdx < n) do
        let cur = amps.[ampIdx % n]
        if firstRound then cur.AddInput(int64 phases.[ampIdx % n])
        cur.AddInput(signal)
        signal <- cur.Run()
        ampIdx <- ampIdx + 1
        if ampIdx >= n then firstRound <- false
    signal

[<EntryPoint>]
let main _ =
    let input = File.ReadAllText("input.txt").Split([|','; '\n'; '\r'; ' '|], StringSplitOptions.RemoveEmptyEntries) |> Array.map int64
    let solve p f = permute p |> List.map (fun x -> runAmps input x f) |> List.max
    printfn "Part 1: %d" (solve [0..4] false)
    printfn "Part 2: %d" (solve [5..9] true)
    0

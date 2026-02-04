
open System
open System.IO

let regIndex = function
    | "a" -> 0 | "b" -> 1 | "c" -> 2 | "d" -> 3 | _ -> -1

let getValue (s:string) (regs:int[]) =
    match Int32.TryParse(s) with
    | true, v -> v
    | _ -> regs.[regIndex s]

let producesClockSignal (aInit:int) (instrs:string[]) =
    let regs = [|aInit;0;0;0|]
    let mutable outCnt = 0
    let mutable expected = 0
    let mutable i = 0
    while i < instrs.Length do
        let parts = instrs.[i].Split(' ')
        match parts.[0] with
        | "cpy" ->
            regs.[regIndex parts.[2]] <- getValue parts.[1] regs
        | "inc" ->
            regs.[regIndex parts.[1]] <- regs.[regIndex parts.[1]] + 1
        | "dec" ->
            regs.[regIndex parts.[1]] <- regs.[regIndex parts.[1]] - 1
        | "jnz" ->
            if getValue parts.[1] regs <> 0 then
                i <- i + getValue parts.[2] regs - 1
        | "out" ->
            if getValue parts.[1] regs <> expected then
                i <- instrs.Length // break loop
            else
                expected <- 1 - expected
                outCnt <- outCnt + 1
                if outCnt >= 100 then
                    i <- instrs.Length // success, break loop
        | _ -> ()
        i <- i + 1
    outCnt >= 100

[<EntryPoint>]
let main _ =
    let instrs = File.ReadAllLines "input.txt"
    let mutable a = 1
    while not (producesClockSignal a instrs) do
        a <- a + 1
    printfn "%d" a
    0

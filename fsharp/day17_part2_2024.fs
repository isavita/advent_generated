
open System
open System.IO
open System.Collections.Generic

type Prog = { A: int64; B: int64; C: int64; Program: int[]; ProgramLen: int }

let computeOperand v a b c =
    match v with
    | 0 | 1 | 2 | 3 -> int64 v
    | 4 -> a
    | 5 -> b
    | 6 -> c
    | _ -> failwithf "Invalid operand %d" v

let simulate (p:Prog) =
    let mutable a = p.A
    let mutable b = p.B
    let mutable c = p.C
    let prog = p.Program
    let len = p.ProgramLen
    let outs = ResizeArray<int>()
    let mutable i = 0
    while i < len do
        let cmd = prog.[i]
        let op = prog.[i+1]
        match cmd with
        | 0 -> a <- a >>> int (computeOperand op a b c)
        | 1 -> b <- b ^^^ int64 op
        | 2 -> b <- computeOperand op a b c % 8L
        | 3 -> if a <> 0L then i <- int op - 2
        | 4 -> b <- b ^^^ c
        | 5 -> outs.Add(int (computeOperand op a b c % 8L))
        | 6 -> b <- a >>> int (computeOperand op a b c)
        | 7 -> c <- a >>> int (computeOperand op a b c)
        | _ -> failwithf "Invalid opcode %d" cmd
        i <- i + 2
    outs.ToArray()

let check (p:Prog) =
    let valid = ResizeArray<int64>()
    let stack = Stack<(int * int64)>()
    let seen = HashSet<(int * int64)>()
    stack.Push((0,0L))
    while stack.Count>0 do
        let depth,score = stack.Pop()
        if seen.Add((depth,score)) then
            if depth = p.ProgramLen then
                valid.Add(score)
            else
                for i in 0uy..7uy do
                    let newScore = int64 i + 8L*score
                    let test = { p with A = newScore }
                    let outArr = simulate test
                    if outArr.Length>0 && outArr.[0] = p.Program.[p.ProgramLen-1-depth] then
                        stack.Push((depth+1,newScore))
    valid.ToArray()

[<EntryPoint>]
let main _ =
    let lines = File.ReadLines("input.txt")
    let mutable a = 0L
    let mutable b = 0L
    let mutable c = 0L
    let mutable prog = [||]
    for line in lines do
        if not (String.IsNullOrWhiteSpace line) then
            let parts = line.Split([|':'|],2)
            if parts.Length=2 then
                let key = parts.[0].Trim()
                let value = parts.[1].Trim()
                match key with
                | "Register A" -> a <- Int64.Parse value
                | "Register B" -> b <- Int64.Parse value
                | "Register C" -> c <- Int64.Parse value
                | "Program" ->
                    prog <- value.Split([|','|],StringSplitOptions.RemoveEmptyEntries)
                            |> Array.map (fun s -> Int32.Parse(s.Trim()))
                | _ -> ()
    let p = { A=a; B=b; C=c; Program=prog; ProgramLen=prog.Length }
    let vals = check p
    if vals.Length=0 then
        printfn "No valid values found"
    else
        vals |> Array.min |> printfn "%d"
    0

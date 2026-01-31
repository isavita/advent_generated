open System
open System.IO

type Instr = { op: string; a: string; b: string }

let getValue (regs:int64[]) (arg:string) =
    if Char.IsLetter arg.[0] then regs.[int arg.[0] - int 'a']
    else Int64.Parse arg

[<EntryPoint>]
let main _ =
    let instrs =
        File.ReadAllLines "input.txt"
        |> Array.map (fun line ->
            let parts = line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
            match parts.Length with
            | 2 -> { op = parts.[0]; a = parts.[1]; b = "" }
            | 3 -> { op = parts.[0]; a = parts.[1]; b = parts.[2] }
            | _ -> failwith "invalid input")
    let regs = Array.zeroCreate<int64> 26
    let mutable lastSound = 0L
    let mutable recovered = 0L
    let mutable ip = 0
    while ip >= 0 && ip < instrs.Length do
        let i = instrs.[ip]
        match i.op with
        | "snd" -> lastSound <- getValue regs i.a
        | "set" -> regs.[int i.a.[0] - int 'a'] <- getValue regs i.b
        | "add" -> regs.[int i.a.[0] - int 'a'] <- regs.[int i.a.[0] - int 'a'] + getValue regs i.b
        | "mul" -> regs.[int i.a.[0] - int 'a'] <- regs.[int i.a.[0] - int 'a'] * getValue regs i.b
        | "mod" -> regs.[int i.a.[0] - int 'a'] <- regs.[int i.a.[0] - int 'a'] % getValue regs i.b
        | "rcv" ->
            if getValue regs i.a <> 0L then
                recovered <- lastSound
                ip <- instrs.Length // exit loop
        | "jgz" ->
            if getValue regs i.a > 0L then
                ip <- ip + int (getValue regs i.b) - 1
        | _ -> ()
        ip <- ip + 1
    printfn "%d" recovered
    0

open System

[<EntryPoint>]
let main _ =
    let instr = IO.File.ReadAllLines("input.txt")
    let regs = Array.zeroCreate<int> 26
    let mutable mulCount = 0
    let mutable ip = 0
    let getVal (s:string) =
        if s.[0] >= 'a' && s.[0] <= 'z' then regs.[int s.[0] - int 'a'] else int s
    while ip >= 0 && ip < instr.Length do
        let p = instr.[ip].Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
        match p.[0] with
        | "set" -> regs.[int p.[1].[0] - int 'a'] <- getVal p.[2]
        | "sub" -> regs.[int p.[1].[0] - int 'a'] <- regs.[int p.[1].[0] - int 'a'] - getVal p.[2]
        | "mul" -> regs.[int p.[1].[0] - int 'a'] <- regs.[int p.[1].[0] - int 'a'] * getVal p.[2]; mulCount <- mulCount + 1
        | "jnz" -> if getVal p.[1] <> 0 then ip <- ip + getVal p.[2] - 1
        | _ -> ()
        ip <- ip + 1
    printfn "%d" mulCount
    0

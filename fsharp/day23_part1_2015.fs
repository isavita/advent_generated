
open System.IO
open System.Collections.Generic

[<EntryPoint>]
let main _ =
    let regs = Dictionary<string,int>(dict["a",0;"b",0])
    let prog = File.ReadAllLines "input.txt"
    let mutable i = 0
    while i < prog.Length do
        let p = prog.[i].Split ' '
        match p.[0] with
        | "hlf" -> regs.[p.[1]] <- regs.[p.[1]] / 2; i <- i + 1
        | "tpl" -> regs.[p.[1]] <- regs.[p.[1]] * 3; i <- i + 1
        | "inc" -> regs.[p.[1]] <- regs.[p.[1]] + 1; i <- i + 1
        | "jmp" -> i <- i + int p.[1]
        | "jie" -> if regs.[p.[1].[0..0]] % 2 = 0 then i <- i + int p.[2] else i <- i + 1
        | "jio" -> if regs.[p.[1].[0..0]] = 1   then i <- i + int p.[2] else i <- i + 1
        | _ -> failwith "bad op"
    printfn "%d" regs.["b"]
    0

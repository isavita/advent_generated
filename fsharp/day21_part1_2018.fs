
open System.IO

type Instr = { Name:string; A:int; B:int; C:int }

let regs = Array.zeroCreate 6
let ipReg,prog =
    let lines = File.ReadAllLines "input.txt"
    let ip = int (lines.[0].[4..])
    let prg =
        lines.[1..]
        |> Array.map (fun l -> let p = l.Split ' ' in { Name=p.[0]; A=int p.[1]; B=int p.[2]; C=int p.[3] })
        |> Array.toList
    ip,prg

let exec () =
    let rec loop ip =
        if ip < 0 || ip >= prog.Length then ()
        else
            let i = prog.[ip]
            regs.[ipReg] <- ip
            match i.Name with
            | "addr" -> regs.[i.C] <- regs.[i.A] + regs.[i.B]
            | "addi" -> regs.[i.C] <- regs.[i.A] + i.B
            | "mulr" -> regs.[i.C] <- regs.[i.A] * regs.[i.B]
            | "muli" -> regs.[i.C] <- regs.[i.A] * i.B
            | "banr" -> regs.[i.C] <- regs.[i.A] &&& regs.[i.B]
            | "bani" -> regs.[i.C] <- regs.[i.A] &&& i.B
            | "borr" -> regs.[i.C] <- regs.[i.A] ||| regs.[i.B]
            | "bori" -> regs.[i.C] <- regs.[i.A] ||| i.B
            | "setr" -> regs.[i.C] <- regs.[i.A]
            | "seti" -> regs.[i.C] <- i.A
            | "gtir" -> regs.[i.C] <- if i.A > regs.[i.B] then 1 else 0
            | "gtri" -> regs.[i.C] <- if regs.[i.A] > i.B then 1 else 0
            | "gtrr" -> regs.[i.C] <- if regs.[i.A] > regs.[i.B] then 1 else 0
            | "eqir" -> regs.[i.C] <- if i.A = regs.[i.B] then 1 else 0
            | "eqri" -> regs.[i.C] <- if regs.[i.A] = i.B then 1 else 0
            | "eqrr" -> regs.[i.C] <- if regs.[i.A] = regs.[i.B] then 1 else 0
            | _ -> ()
            let nip = regs.[ipReg] + 1
            if nip = 28 then printfn "%d" regs.[5] else loop nip
    loop 0

[<EntryPoint>]
let main _ = exec(); 0

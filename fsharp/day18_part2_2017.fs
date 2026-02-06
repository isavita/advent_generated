
open System
open System.Collections.Generic
open System.IO

type Arg = R of int | V of int64
type Ins = Snd of Arg | Set of int * Arg | Add of int * Arg | Mul of int * Arg | Mod of int * Arg | Rcv of int | Jgz of Arg * Arg
type St = { Id: int; Regs: int64[]; mutable Pc: int; Q: Queue<int64>; mutable Sc: int; mutable Stat: int }

let gv (r: int64[]) = function R i -> r.[i] | V v -> v
let pa (s: string) = match Int64.TryParse s with true, v -> V v | _ -> R (int s.[0] - int 'a')

[<EntryPoint>]
let main _ =
    let insts = File.ReadAllLines "input.txt" |> Array.map (fun l ->
        let p = l.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
        let r i = int p.[i].[0] - int 'a'
        match p.[0] with
        | "snd" -> Snd(pa p.[1]) | "set" -> Set(r 1, pa p.[2]) | "add" -> Add(r 1, pa p.[2])
        | "mul" -> Mul(r 1, pa p.[2]) | "mod" -> Mod(r 1, pa p.[2]) | "rcv" -> Rcv(r 1)
        | "jgz" -> Jgz(pa p.[1], pa p.[2]) | _ -> failwith "")
    let mk id =
        let rs = Array.zeroCreate 26 in rs.[int 'p' - int 'a'] <- int64 id
        { Id=id; Regs=rs; Pc=0; Q=Queue(); Sc=0; Stat=0 }
    let p0, p1 = mk 0, mk 1
    let step me ot =
        match insts.[me.Pc] with
        | Snd x -> ot.Q.Enqueue(gv me.Regs x); if me.Id = 1 then me.Sc <- me.Sc + 1
                   if ot.Stat = 1 then ot.Stat <- 0
                   me.Pc <- me.Pc + 1
        | Set(r, x) -> me.Regs.[r] <- gv me.Regs x; me.Pc <- me.Pc + 1
        | Add(r, x) -> me.Regs.[r] <- me.Regs.[r] + gv me.Regs x; me.Pc <- me.Pc + 1
        | Mul(r, x) -> me.Regs.[r] <- me.Regs.[r] * gv me.Regs x; me.Pc <- me.Pc + 1
        | Mod(r, x) -> me.Regs.[r] <- me.Regs.[r] % gv me.Regs x; me.Pc <- me.Pc + 1
        | Rcv r -> if me.Q.Count > 0 then me.Regs.[r] <- me.Q.Dequeue(); me.Pc <- me.Pc + 1 else me.Stat <- 1
        | Jgz(x, y) -> if gv me.Regs x > 0L then me.Pc <- me.Pc + int (gv me.Regs y) else me.Pc <- me.Pc + 1
        if me.Pc < 0 || me.Pc >= insts.Length then me.Stat <- 2
    while p0.Stat = 0 || p1.Stat = 0 do
        while p0.Stat = 0 do step p0 p1
        while p1.Stat = 0 do step p1 p0
    printfn "%d" p1.Sc
    0

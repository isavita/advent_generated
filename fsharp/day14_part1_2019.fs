
open System
open System.Collections.Generic
open System.IO

type Chem = { Name:string; Amount:int }

let parse (s:string) =
    let a = s.Split(' ')
    {Name=a.[1]; Amount=int a.[0]}

let lines = File.ReadAllLines "input.txt"

let reactions = Dictionary<string,Chem>()
let ingredients = Dictionary<string,Chem list>()
let all = HashSet<string>()

for l in lines do
    let p = l.Split " => "
    let ins = p.[0].Split ", " |> Array.map parse |> Array.toList
    let out = parse p.[1]
    reactions.[out.Name] <- out
    ingredients.[out.Name] <- ins
    ins |> List.iter (fun c -> all.Add c.Name |> ignore)
    all.Add out.Name |> ignore

for c in all do
    if not (reactions.ContainsKey c) then
        reactions.[c] <- {Name=c; Amount=0}

let rec ore chem qty (surplus:Dictionary<string,int64>) =
    if chem = "ORE" then qty
    else
        let s = if surplus.ContainsKey chem then surplus.[chem] else 0L
        if s >= qty then
            surplus.[chem] <- s - qty
            0L
        else
            let need = qty - s
            surplus.[chem] <- 0L
            let r = reactions.[chem]
            let times = (need + int64 r.Amount - 1L) / int64 r.Amount
            let mutable o = 0L
            for i in ingredients.[chem] do
                o <- o + ore i.Name (int64 i.Amount * times) surplus
            surplus.[chem] <- surplus.[chem] + times * int64 r.Amount - need
            o

[<EntryPoint>]
let main _ =
    printfn "%d" (ore "FUEL" 1L (Dictionary<string,int64>()))
    0

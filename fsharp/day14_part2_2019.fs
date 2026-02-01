
open System
open System.Collections.Generic
open System.IO

type Reaction = { OutName:string; OutQty:int64; Inps:(string*int64)list }

let parse (s:string) =
    let toPair (t:string) =
        let a = t.Trim().Split(' ')
        (a.[1], int64 a.[0])
    let p = s.Split("=>")
    let outs = toPair p.[1]
    let ins = p.[0].Split(',') |> Array.map toPair |> Array.toList
    { OutName=fst outs; OutQty=snd outs; Inps=ins }

let reactions = Dictionary<string,Reaction>()
let surplus = Dictionary<string,int64>()

let rec oreNeed chem need =
    if chem = "ORE" then need
    else
        let have = if surplus.ContainsKey chem then surplus.[chem] else 0L
        if have >= need then
            surplus.[chem] <- have - need
            0L
        else
            let need = need - have
            surplus.[chem] <- 0L
            let r = reactions.[chem]
            let times = (need + r.OutQty - 1L) / r.OutQty
            let o = r.Inps |> List.sumBy (fun (n,q) -> oreNeed n (q * times))
            surplus.[chem] <- surplus.TryGetValue(chem) |> snd |> (+) (times * r.OutQty - need)
            o

[<EntryPoint>]
let main _ =
    File.ReadAllLines "input.txt"
    |> Array.iter (parse >> fun r -> reactions.[r.OutName] <- r)

    let one = oreNeed "FUEL" 1L
    let ore = 1000000000000L
    let mutable lo = 0L
    let mutable hi = (ore / one) * 2L
    let mutable best = 0L
    while lo <= hi do
        let mid = lo + (hi - lo) / 2L
        surplus.Clear()
        if oreNeed "FUEL" mid <= ore then
            best <- mid
            lo <- mid + 1L
        else
            hi <- mid - 1L
    printfn "%d" best
    0

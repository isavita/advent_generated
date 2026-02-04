
open System
open System.IO

type Snail = { mutable Value:int; mutable Left:Snail option; mutable Right:Snail option }

let isRegular n = n.Left.IsNone

let rec parse (s:string) (i:byref<int>) =
    if s.[i] = '[' then
        i <- i + 1
        let l = parse s &i
        i <- i + 1 // ','
        let r = parse s &i
        i <- i + 1 // ']'
        { Value = -1; Left = Some l; Right = Some r }
    else
        let mutable v = 0
        while i < s.Length && Char.IsDigit s.[i] do
            v <- v * 10 + int s.[i] - int '0'
            i <- i + 1
        { Value = v; Left = None; Right = None }

let rec addLeft n v =
    if isRegular n then n.Value <- n.Value + v
    else addLeft (Option.get n.Left) v

let rec addRight n v =
    if isRegular n then n.Value <- n.Value + v
    else addRight (Option.get n.Right) v

let rec explode n depth (addL:byref<int>) (addR:byref<int>) =
    if isRegular n then false
    elif depth = 4 then
        addL <- (Option.get n.Left).Value
        addR <- (Option.get n.Right).Value
        n.Left <- None; n.Right <- None; n.Value <- 0
        true
    else
        let mutable al = 0
        let mutable ar = 0
        if explode (Option.get n.Left) (depth+1) &al &ar then
            if ar <> 0 then addLeft (Option.get n.Right) ar
            addL <- al; addR <- 0; true
        elif explode (Option.get n.Right) (depth+1) &al &ar then
            if al <> 0 then addRight (Option.get n.Left) al
            addL <- 0; addR <- ar; true
        else false

let rec split n =
    if isRegular n then
        if n.Value < 10 then false
        else
            let l = n.Value / 2
            let r = (n.Value + 1) / 2
            n.Left <- Some { Value = l; Left = None; Right = None }
            n.Right <- Some { Value = r; Left = None; Right = None }
            n.Value <- -1
            true
    else split (Option.get n.Left) || split (Option.get n.Right)

let rec reduce n =
    let mutable changed = true
    while changed do
        let mutable al = 0
        let mutable ar = 0
        if explode n 0 &al &ar then ()
        elif split n then ()
        else changed <- false

let add a b =
    let n = { Value = -1; Left = Some a; Right = Some b }
    reduce n
    n

let rec magnitude n =
    if isRegular n then int64 n.Value
    else 3L * magnitude (Option.get n.Left) + 2L * magnitude (Option.get n.Right)

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let nums = lines |> Array.map (fun l -> let mutable i = 0 in parse l &i)
    let res = Array.reduce add nums
    printfn "%d" (magnitude res)
    0

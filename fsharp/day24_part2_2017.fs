
open System
open System.IO

type Component = { a:int; b:int }

let comps = 
    File.ReadAllLines("input.txt")
    |> Array.map (fun l -> 
        let p = l.Split('/')
        { a = int p.[0]; b = int p.[1] })

let n = comps.Length
let used = Array.create n false
let mutable bestLen = 0
let mutable bestStr = 0

let rec build port len str =
    if len > bestLen || (len = bestLen && str > bestStr) then
        bestLen <- len
        bestStr <- str
    for i = 0 to n-1 do
        if not used.[i] then
            let c = comps.[i]
            if c.a = port || c.b = port then
                used.[i] <- true
                let nxt = if c.a = port then c.b else c.a
                build nxt (len+1) (str + c.a + c.b)
                used.[i] <- false

[<EntryPoint>]
let main _ =
    build 0 0 0
    printfn "%d" bestStr
    0

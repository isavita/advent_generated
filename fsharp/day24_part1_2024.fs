
open System
open System.IO
open System.Collections.Generic

let lines = File.ReadAllLines "input.txt" |> Array.toList

let (initials, gates) =
    let sep = List.findIndex (fun (s:string) -> s = "") lines
    (lines |> List.take sep, lines |> List.skip (sep + 1))

let wires = Dictionary<string,int>()

for s in initials do
    let [|a;b|] = s.Split ": "
    wires.[a] <- int b

let parsedGates =
    gates
    |> List.filter (fun s -> s <> "")
    |> List.map (fun (s:string) ->
        let p = s.Split ' '
        (p.[0], p.[1], p.[2], p.[4]))

let rec compute () =
    let mutable changed = false
    for (i1,op,i2,out) in parsedGates do
        if not (wires.ContainsKey out) && wires.ContainsKey i1 && wires.ContainsKey i2 then
            let v1 = wires.[i1]
            let v2 = wires.[i2]
            let res =
                match op with
                | "AND" -> v1 &&& v2
                | "OR"  -> v1 ||| v2
                | "XOR" -> v1 ^^^ v2
                | _ -> 0
            wires.[out] <- res
            changed <- true
    if changed then compute ()

compute ()

let zBits =
    wires
    |> Seq.filter (fun kv -> kv.Key.StartsWith "z")
    |> Seq.sortByDescending (fun kv -> kv.Key)
    |> Seq.map (fun kv -> if kv.Value = 1 then '1' else '0')
    |> String.Concat

let result = Convert.ToInt64(zBits, 2)
printfn "%d" result

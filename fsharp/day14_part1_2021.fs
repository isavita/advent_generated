
open System
open System.IO
open System.Collections.Generic

type Rule = { pair: string; insert: char }

let readRules (lines: string seq) =
    lines
    |> Seq.skipWhile (fun l -> l.Trim() <> "")
    |> Seq.skip 1
    |> Seq.map (fun line ->
        let parts = line.Split "->"
        let pair = parts.[0].Trim()
        let insert = parts.[1].Trim().Chars 0
        { pair = pair; insert = insert })

let applyRules (polymer: string) (rules: Rule list) =
    let dict = rules |> List.map (fun r -> (r.pair, r.insert)) |> dict
    let sb = new System.Text.StringBuilder()
    for i = 0 to polymer.Length - 2 do
        sb.Append(polymer.[i]) |> ignore
        match dict.TryGetValue(polymer.Substring(i, 2)) with
        | true, c -> sb.Append(c) |> ignore
        | _ -> ()
    sb.Append(polymer.[polymer.Length - 1]) |> ignore
    sb.ToString()

let countChars (s: string) =
    s
    |> Seq.groupBy id
    |> Seq.map (fun (k, g) -> (k, Seq.length g))
    |> Seq.toList

let main() =
    try
        let lines = File.ReadAllLines("input.txt")
        let polymer = lines.[0].Trim()
        let rules = readRules lines |> Seq.toList
        let mutable currentPolymer = polymer
        for _ = 1 to 10 do
            currentPolymer <- applyRules currentPolymer rules
        let counts = countChars currentPolymer
        let max = counts |> List.maxBy snd |> snd
        let min = counts |> List.minBy snd |> snd
        printfn "%d" (max - min)
    with
    | ex -> printfn "Error: %s" ex.Message

main()

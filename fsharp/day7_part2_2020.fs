
open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.IO

type BagRule = { Color: string; Count: int }

let ruleRegex = new Regex(@"(\d+) (\w+ \w+) bags?[,.]")

let parseLine (line: string) =
    let parts = line.Split([| " bags contain " |], StringSplitOptions.None)
    let container = parts.[0]
    let contents = parts.[1]
    if contents = "no other bags." then
        None
    else
        let rules =
            ruleRegex.Matches(contents)
            |> Seq.cast<Match>
            |> Seq.map (fun m ->
                let count = int m.Groups.[1].Value
                let color = m.Groups.[2].Value
                { Color = color; Count = count })
            |> List.ofSeq
        Some (container, rules)

let countBags (color: string) (rules: Map<string, BagRule list>) =
    let rec count color =
        match rules.TryFind color with
        | None -> 1
        | Some rules ->
            1 + (rules |> List.sumBy (fun rule -> rule.Count * count rule.Color))
    count color - 1

let main () =
    let rules =
        File.ReadAllLines("input.txt")
        |> Array.choose parseLine
        |> Map.ofArray
    let totalBags = countBags "shiny gold" rules
    printfn "%d" totalBags

main ()

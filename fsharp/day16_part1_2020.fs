
open System
open System.IO
open System.Text.RegularExpressions

type Rule = { Name: string; Ranges: (int * int) list }

let isValid (value: int) (rule: Rule) =
    rule.Ranges |> List.exists (fun (min, max) -> value >= min && value <= max)

let isValidForAnyRule (value: int) (rules: Rule list) =
    rules |> List.exists (isValid value)

let parseRule (line: string) =
    let matchRule = Regex.Match(line, @"([^:]+): (\d+)-(\d+) or (\d+)-(\d+)")
    if matchRule.Success then
        let name = matchRule.Groups.[1].Value
        let range1Min = int matchRule.Groups.[2].Value
        let range1Max = int matchRule.Groups.[3].Value
        let range2Min = int matchRule.Groups.[4].Value
        let range2Max = int matchRule.Groups.[5].Value
        Some { Name = name; Ranges = [(range1Min, range1Max); (range2Min, range2Max)] }
    else
        None

let main () =
    try
        let lines = File.ReadAllLines("input.txt")
        let mutable rules = []
        let mutable errorRate = 0
        let mutable scanningRules = true

        for line in lines do
            if line.Trim() <> "" then
                if line.Contains("your ticket:") || line.Contains("nearby tickets:") then
                    scanningRules <- false
                elif scanningRules then
                    match parseRule line with
                    | Some rule -> rules <- rule :: rules
                    | None -> ()
                else
                    let values = line.Split(',') |> Array.map int
                    for value in values do
                        if not (isValidForAnyRule value rules) then
                            errorRate <- errorRate + value

        printfn "%d" errorRate
    with
    | ex -> printfn "Error: %s" ex.Message

main ()


open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

type Option = { SubRules: int list }

type Rule = 
    | Unresolved of Option list
    | Resolved of string list

let resolve (rules: Map<int, Rule>) ruleNum =
    let rec resolveRule rule =
        match rule with
        | Resolved strs -> strs
        | Unresolved options ->
            options
            |> List.map (fun opt ->
                opt.SubRules
                |> List.map (fun subRule -> resolveRule (Map.find subRule rules))
                |> List.fold (fun strs subRuleStrs ->
                    strs |> List.collect (fun str -> subRuleStrs |> List.map (fun subStr -> str + subStr))) [""])
            |> List.collect id
            |> List.distinct

    resolveRule (Map.find ruleNum rules)

let solve (input: string) =
    let parts = input.Split([| "\r\n\r\n"; "\n\n" |], StringSplitOptions.None)
    let rulePart = parts.[0]
    let messagePart = parts.[1]

    let rules =
        rulePart.Split([| "\r\n"; "\n" |], StringSplitOptions.None)
        |> Array.map (fun line ->
            let matchResult = Regex.Match(line, @"(\d+): (.*)")
            let ruleNum = int matchResult.Groups.[1].Value
            let ruleDef = matchResult.Groups.[2].Value
            if ruleDef.StartsWith "\"" && ruleDef.EndsWith "\"" then
                (ruleNum, Resolved [ruleDef.Substring(1, ruleDef.Length - 2)])
            else
                let options =
                    ruleDef.Split '|'
                    |> Array.map (fun opt ->
                        let subRules =
                            opt.Trim().Split ' '
                            |> Array.map int
                            |> List.ofArray
                        { SubRules = subRules })
                    |> List.ofArray
                (ruleNum, Unresolved options))
        |> Map.ofArray

    let messages =
        messagePart.Split([| "\r\n"; "\n" |], StringSplitOptions.None)
        |> Array.filter (not << String.IsNullOrEmpty)
        |> List.ofArray

    let rule42 = resolve rules 42
    let rule31 = resolve rules 31
    let chunkLen = rule42.[0].Length

    messages
    |> List.filter (fun msg -> msg.Length % chunkLen = 0 && msg.Length / chunkLen >= 3)
    |> List.filter (fun msg ->
        let numChunks = msg.Length / chunkLen
        let count42 =
            [0 .. numChunks - 1]
            |> List.takeWhile (fun k ->
                let chunk = msg.Substring(k * chunkLen, chunkLen)
                List.contains chunk rule42)
            |> List.length
        let count31 =
            [count42 .. numChunks - 1]
            |> List.takeWhile (fun k ->
                let chunk = msg.Substring(k * chunkLen, chunkLen)
                List.contains chunk rule31)
            |> List.length
        count42 + count31 = numChunks && count31 >= 1 && count42 > count31)
    |> List.length

let main() =
    try
        let input = File.ReadAllText "input.txt"
        let result = solve input
        printfn "%d" result
    with
    | ex -> printfn "Error: %s" ex.Message

main()

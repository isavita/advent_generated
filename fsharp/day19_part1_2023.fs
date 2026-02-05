
open System
open System.IO
open System.Collections.Generic

type Rule = { Category: char; Op: char; Value: int; DestIdx: int }
type Workflow = { Name: string; Rules: ResizeArray<Rule> }
type Part = { x: int; m: int; a: int; s: int }

let workflows = ResizeArray<Workflow>()
let nameToIdx = Dictionary<string,int>()
let parts = ResizeArray<Part>()

let getIdx name =
    if name = "A" then -1
    elif name = "R" then -2
    else
        match nameToIdx.TryGetValue(name) with
        | true, i -> i
        | _ ->
            let idx = workflows.Count
            workflows.Add({ Name = name; Rules = ResizeArray() })
            nameToIdx.[name] <- idx
            idx

let parseRule (wf:Workflow) (token:string) =
    let colon = token.IndexOf(':')
    let (cat, op, value, dest) =
        if colon >= 0 then
            let cat = token.[0]
            let op = token.[1]
            let value = Int32.Parse(token.Substring(2, colon-2))
            let dest = token.Substring(colon+1)
            (cat, op, value, dest)
        else
            ('\000', '\000', 0, token)
    wf.Rules.Add({ Category = cat; Op = op; Value = value; DestIdx = getIdx dest })

let parseInput file =
    let lines = File.ReadAllLines file
    let mutable parsingWf = true
    for line in lines do
        let line = line.Trim()
        if line = "" then parsingWf <- false
        elif parsingWf then
            let brace = line.IndexOf('{')
            if brace >= 0 then
                let name = line.Substring(0, brace)
                let idx = getIdx name
                let wf = workflows.[idx]
                wf.Rules.Clear()
                let inner = line.Substring(brace+1).TrimEnd('}')
                for token in inner.Split([|','|], StringSplitOptions.RemoveEmptyEntries) do
                    parseRule wf (token.Trim())
        else
            if parts.Count < 600 then
                let inner = line.Substring(line.IndexOf('{')+1).TrimEnd('}')
                let mutable p = { x=0; m=0; a=0; s=0 }
                for kv in inner.Split([|','|], StringSplitOptions.RemoveEmptyEntries) do
                    let kv = kv.Split('=')
                    if kv.Length = 2 then
                        let key = kv.[0].Trim()
                        let v = Int32.Parse(kv.[1].Trim())
                        match key with
                        | "x" -> p <- { p with x = v }
                        | "m" -> p <- { p with m = v }
                        | "a" -> p <- { p with a = v }
                        | "s" -> p <- { p with s = v }
                        | _ -> ()
                parts.Add(p)

let evalPart (part:Part) startIdx =
    let mutable idx = startIdx
    let mutable ok = false
    while idx >= 0 && not ok do
        let wf = workflows.[idx]
        let mutable matched = false
        for rule in wf.Rules do
            if not matched then
                let cond =
                    if rule.Category = '\000' then true
                    else
                        let v =
                            match rule.Category with
                            | 'x' -> part.x
                            | 'm' -> part.m
                            | 'a' -> part.a
                            | 's' -> part.s
                            | _   -> 0
                        if rule.Op = '<' then v < rule.Value else v > rule.Value
                if cond then
                    idx <- rule.DestIdx
                    matched <- true
        if not matched then idx <- -2
        if idx = -1 then ok <- true
        elif idx = -2 then ok <- false
    ok

[<EntryPoint>]
let main _ =
    parseInput "input.txt"
    let inIdx = getIdx "in"
    let mutable total = 0L
    for p in parts do
        if evalPart p inIdx then
            total <- total + int64 (p.x + p.m + p.a + p.s)
    printfn "%d" total
    0

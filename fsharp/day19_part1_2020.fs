
open System
open System.IO
open System.Text.RegularExpressions

// Represents a sequence of sub-rule IDs (e.g., "1 2")
type SubRuleSeq = { ids: int array; count: int }

// Represents a single rule
type RuleType =
    | RuleChar of char
    | RuleSeq of SubRuleSeq
    | RuleAlt of SubRuleSeq * SubRuleSeq

type Rule = { ruleType: RuleType; defined: bool }

// Global storage for rules and messages
let rules = Array.init 200 (fun _ -> { ruleType = RuleSeq { ids = Array.zeroCreate 0; count = 0 }; defined = false })
let mutable maxRuleNum = -1

// Parsing Functions
let parseRules (lines: string seq) =
    let parseLine (line: string) =
        let parts = line.Split(':')
        if parts.Length = 2 then
            let ruleId = int parts.[0]
            if ruleId >= 0 && ruleId < 200 then
                maxRuleNum <- max maxRuleNum ruleId
                let definition = parts.[1].Trim()
                if definition.StartsWith("\"") && definition.Length = 3 then
                    rules.[ruleId] <- { ruleType = RuleChar definition.[1]; defined = true }
                else
                    let altParts = definition.Split('|')
                    if altParts.Length = 2 then
                        let seq1Ids = altParts.[0].Trim().Split(' ') |> Array.map int
                        let seq2Ids = altParts.[1].Trim().Split(' ') |> Array.map int
                        rules.[ruleId] <- { ruleType = RuleAlt ({ ids = seq1Ids; count = seq1Ids.Length }, { ids = seq2Ids; count = seq2Ids.Length }); defined = true }
                    else
                        let seqIds = definition.Split(' ') |> Array.map int
                        rules.[ruleId] <- { ruleType = RuleSeq { ids = seqIds; count = seqIds.Length }; defined = true }

    lines |> Seq.iter parseLine

// Matching Function
let rec matchRule (ruleId: int) (message: string) (pos: int) =
    if pos < 0 || pos > message.Length then -1
    elif ruleId < 0 || ruleId > maxRuleNum || not rules.[ruleId].defined then -1
    else
        match rules.[ruleId].ruleType with
        | RuleChar c ->
            if pos < message.Length && message.[pos] = c then pos + 1
            else -1
        | RuleSeq seq ->
            let mutable currentPos = pos
            let mutable success = true
            for i = 0 to seq.count - 1 do
                currentPos <- matchRule seq.ids.[i] message currentPos
                if currentPos = -1 then success <- false
            if success then currentPos else -1
        | RuleAlt (seq1, seq2) ->
            let mutable currentPos1 = pos
            let mutable success1 = true
            for i = 0 to seq1.count - 1 do
                currentPos1 <- matchRule seq1.ids.[i] message currentPos1
                if currentPos1 = -1 then success1 <- false
            if success1 then currentPos1
            else
                let mutable currentPos2 = pos
                let mutable success2 = true
                for i = 0 to seq2.count - 1 do
                    currentPos2 <- matchRule seq2.ids.[i] message currentPos2
                    if currentPos2 = -1 then success2 <- false
                if success2 then currentPos2 else -1

// Main Function
let main () =
    try
        let lines = File.ReadAllLines "input.txt"
        let ruleLines = lines |> Seq.takeWhile (fun line -> line.Trim() <> "")
        parseRules ruleLines
        let messageLines = lines |> Seq.skipWhile (fun line -> line.Trim() <> "") |> Seq.skip 1 |> Seq.map (fun line -> line.Trim()) |> Seq.filter (fun line -> line <> "")
        let validCount =
            messageLines
            |> Seq.filter (fun message ->
                if rules.[0].defined then
                    let endPos = matchRule 0 message 0
                    endPos = message.Length
                else false)
            |> Seq.length
        printfn "%d" validCount
    with ex ->
        printfn "An error occurred: %s" ex.Message

main ()

open System
open System.IO
open System.Collections.Generic

type GateDef = { a: string; op: string; b: string }
type Gate = { mutable def: GateDef; mutable output: string }

let to2 i = sprintf "%02d" i

[<EntryPoint>]
let main argv =
    let MAX_NAME_LEN = 8
    let NUM_PAIRS = 4

    let mutable gates = new List<Gate>()
    let input = File.ReadAllText("input.txt")
    let delimIdx = input.IndexOf("\n\n")
    let definitions =
        if delimIdx >= 0 then input.Substring(delimIdx + 2) else input

    let mutable numZ = 0
    for rawLine in definitions.Split('\n') do
        let line = rawLine.Trim('\r')
        if line.Length > 0 then
            let parts = line.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
            if parts.Length >= 5 then
                let g = { def = { a = parts.[0]; op = parts.[1]; b = parts.[2] }; output = parts.[4] }
                gates.Add(g)
                if g.output.StartsWith("z") then numZ <- numZ + 1

    let findOutputByGate (a1:string) (op:string) (b1:string) =
        gates |> Seq.tryFind (fun g -> g.def.op = op &&
                                        ((g.def.a = a1 && g.def.b = b1) || (g.def.a = b1 && g.def.b = a1)))
              |> Option.map (fun g -> g.output)

    let findGateByOutput (output:string) =
        gates |> Seq.tryFind (fun g -> g.output = output)

    let swapGateOutputs (out1:string) (out2:string) =
        for g in gates do
            if g.output = out1 then g.output <- out2
            elif g.output = out2 then g.output <- out1

    let swappedPairs = new List<string>(NUM_PAIRS * 2)
    let mutable pairCount = 0

    while pairCount < NUM_PAIRS do
        let mutable carry = ""
        let mutable swappedThisRound = false
        let mutable i = 0
        while i < numZ && not swappedThisRound do
            let xi = "x" + to2 i
            let yi = "y" + to2 i
            let zi = "z" + to2 i

            let mutable adderOpt : string option = None
            let mutable nextCarryOpt : string option = None

            if i = 0 then
                adderOpt <- findOutputByGate xi "XOR" yi
                nextCarryOpt <- findOutputByGate xi "AND" yi
            else
                let bitOpt = findOutputByGate xi "XOR" yi
                if bitOpt.IsSome && carry <> "" then
                    adderOpt <- findOutputByGate bitOpt.Value "XOR" carry
                    if adderOpt.IsSome then
                        let c1Opt = findOutputByGate xi "AND" yi
                        let c2Opt = findOutputByGate bitOpt.Value "AND" carry
                        if c1Opt.IsSome && c2Opt.IsSome then
                            nextCarryOpt <- findOutputByGate c1Opt.Value "OR" c2Opt.Value

            let mutable swapped = false
            if adderOpt.IsSome && adderOpt.Value <> zi then
                swappedPairs.Add(adderOpt.Value)
                swappedPairs.Add(zi)
                swapGateOutputs adderOpt.Value zi
                swapped <- true
            else
                if adderOpt.IsNone then
                    let gateZOpt = findGateByOutput zi
                    let bitOpt = findOutputByGate xi "XOR" yi
                    if gateZOpt.IsSome && bitOpt.IsSome && carry <> "" then
                        let gateZ = gateZOpt.Value
                        let aOpt = findOutputByGate gateZ.def.a "XOR" carry
                        if aOpt.IsSome then
                            swappedPairs.Add(bitOpt.Value)
                            swappedPairs.Add(gateZ.def.a)
                            swapGateOutputs bitOpt.Value gateZ.def.a
                            swapped <- true
                        else
                            let bOpt = findOutputByGate gateZ.def.b "XOR" carry
                            if bOpt.IsSome then
                                swappedPairs.Add(bitOpt.Value)
                                swappedPairs.Add(gateZ.def.b)
                                swapGateOutputs bitOpt.Value gateZ.def.b
                                swapped <- true

            if swapped then
                pairCount <- pairCount + 1
                swappedThisRound <- true
            carry <- (if nextCarryOpt.IsSome then nextCarryOpt.Value else "")

            i <- i + 1

    swappedPairs.Sort(StringComparer.Ordinal)
    let result = String.Join(",", swappedPairs.ToArray())
    printfn "%s" result
    0
open System
open System.IO
open System.Collections.Generic

type Valve = { Id: string; mutable Flow: int; mutable Index: int; mutable OpenMaskIndex: int }

[<EntryPoint>]
let main _ =
    let maxValves = 60
    let timeLimit = 26
    let infinity = 1_000_000
    let maskSize = 1 <<< 16

    let valves : Valve option array = Array.create maxValves None
    let dist = Array2D.create maxValves maxValves infinity
    for i in 0 .. maxValves - 1 do dist.[i, i] <- 0
    let mutable numValves = 0
    let openValveIndices : int array = Array.zeroCreate maxValves
    let mutable numOpenValves = 0
    let valveDict = Dictionary<string,int>()

    let ensureValve (id:string) =
        if valveDict.ContainsKey(id) then valveDict.[id]
        else
            let idx = numValves
            valveDict.Add(id, idx)
            valves.[idx] <- Some { Id = id; Flow = 0; Index = idx; OpenMaskIndex = -1 }
            numValves <- numValves + 1
            idx

    if not (File.Exists("input.txt")) then
        printfn "Error: input.txt not found."
        0
    else
        let lines = File.ReadAllLines("input.txt")
        for line in lines do
            if not (String.IsNullOrWhiteSpace(line)) then
                let idxValve = line.IndexOf("Valve ")
                let idStart = idxValve + "Valve ".Length
                let endId = line.IndexOf(" has flow rate=", idStart)
                let valveId = line.Substring(idStart, endId - idStart)
                let flowStart = endId + " has flow rate=".Length
                let semicolon = line.IndexOf(';', flowStart)
                let flowStr = if semicolon >= 0 then line.Substring(flowStart, semicolon - flowStart) else line.Substring(flowStart)
                let flowRate = int flowStr
                let curIdx = ensureValve valveId
                match valves.[curIdx] with
                | Some v -> v.Flow <- flowRate
                | None -> ()

                let afterSemi = if semicolon >= 0 then line.Substring(semicolon+1).Trim() else ""
                let neighborsPart = afterSemi.Replace("tunnels lead to valves ", "").Replace("tunnel leads to valve ", "")
                let neighborNames = if String.IsNullOrWhiteSpace(neighborsPart) then [||] else neighborsPart.Split(", ", StringSplitOptions.None)
                for neighbor in neighborNames do
                    if neighbor <> "" then
                        let nIdx = ensureValve neighbor
                        dist.[curIdx, nIdx] <- 1

        for k in 0 .. numValves - 1 do
            for i in 0 .. numValves - 1 do
                let dik = dist.[i, k]
                if dik < infinity then
                    for j in 0 .. numValves - 1 do
                        let dkj = dist.[k, j]
                        if dkj < infinity then
                            let nd = dik + dkj
                            if nd < dist.[i, j] then dist.[i, j] <- nd

        for i in 0 .. numValves - 1 do
            match valves.[i] with
            | Some v when v.Flow > 0 ->
                v.OpenMaskIndex <- numOpenValves
                openValveIndices.[numOpenValves] <- i
                numOpenValves <- numOpenValves + 1
            | _ -> ()

        let mutable startIdx = -1
        for i in 0 .. numValves - 1 do
            match valves.[i] with
            | Some v when v.Id = "AA" -> startIdx <- i
            | _ -> ()

        if startIdx < 0 then
            printfn "Start valve 'AA' not found."
            0
        else
            let totalMasks = 1 <<< numOpenValves
            let memoSize = maxValves * (timeLimit + 1) * maskSize
            let memo : int array = Array.create memoSize -1

            let inline memoIndex (curr:int) (t:int) (mask:int) =
                ((curr * (timeLimit + 1) + t) * maskSize) + mask

            let rec Solve (currIdx:int) (timeLeft:int) (openMask:int) : int =
                if timeLeft <= 0 then 0
                else
                    let idx = memoIndex currIdx timeLeft openMask
                    let cached = memo.[idx]
                    if cached <> -1 then cached
                    else
                        let mutable maxP = 0
                        for i in 0 .. numOpenValves - 1 do
                            if ((openMask >>> i) &&& 1) = 1 then
                                let nextValveIdx = openValveIndices.[i]
                                let travelTime = dist.[currIdx, nextValveIdx]
                                let timeAfter = timeLeft - travelTime - 1
                                if timeAfter > 0 then
                                    let vOpt = valves.[nextValveIdx]
                                    let flowHere = match vOpt with Some v -> v.Flow | None -> 0
                                    let currentPressure = flowHere * timeAfter
                                    let maskCleared = openMask &&& (~~~(1 <<< i))
                                    let press = currentPressure + Solve nextValveIdx timeAfter maskCleared
                                    if press > maxP then maxP <- press
                        memo.[idx] <- maxP
                        maxP

            let mutable maxTotal = 0
            for myMask in 0 .. totalMasks - 1 do
                let elephantMask = (totalMasks - 1) ^^^ myMask
                let myPressure = Solve startIdx timeLimit myMask
                let elephantPressure = Solve startIdx timeLimit elephantMask
                let sum = myPressure + elephantPressure
                if sum > maxTotal then maxTotal <- sum

            printfn "%d" maxTotal
            0
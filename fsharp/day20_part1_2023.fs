open System
open System.Collections.Generic
open System.IO

type PulseValue =
    | Low
    | High

type Pulse = { value: PulseValue; fromName: string; toName: string }

type Module = {
    mutable name: string
    mutable prefix: char
    mutable destinations: string list
    mutable state: bool
    mutable memory: Dictionary<string, PulseValue>
}

let parseInput (input: string[]) =
    let modules = new Dictionary<string, Module>()
    for line in input do
        if not (String.IsNullOrWhiteSpace(line)) then
            let parts = line.Split([| " -> " |], StringSplitOptions.None)
            if parts.Length >= 2 then
                let left = parts.[0]
                let right = parts.[1]
                let mutable name = left
                let mutable pref = '\000'
                if left.Length > 0 then
                    if left.[0] = '%' then pref <- '%' ; name <- left.Substring(1)
                    elif left.[0] = '&' then pref <- '&' ; name <- left.Substring(1)
                let destinations = (right.Split([| ", " |], StringSplitOptions.None)) |> Array.toList
                let m = { name = name; prefix = pref; destinations = destinations; state = false; memory = new Dictionary<string, PulseValue>() }
                modules.Add(m.name, m)
    for m in modules.Values do
        for destName in m.destinations do
            let ok, destModule = modules.TryGetValue(destName)
            if ok && destModule.prefix = '&' then
                destModule.memory.[m.name] <- Low
    modules

let pushButton (modules: Dictionary<string, Module>) (startPulse: Pulse) (numCycle: int) =
    let mutable cntLow = 0
    let mutable cntHigh = 0
    let pulseQueue = new List<Pulse>()
    for _ in 0 .. numCycle - 1 do
        pulseQueue.Add(startPulse)
        while pulseQueue.Count > 0 do
            let pulse = pulseQueue.[0]
            pulseQueue.RemoveAt(0)
            match pulse.value with
            | Low -> cntLow <- cntLow + 1
            | High -> cntHigh <- cntHigh + 1
            if not (modules.ContainsKey(pulse.toName)) then
                ()
            else
                let m = modules.[pulse.toName]
                let mutable proceed = true
                let newPulseValue =
                    match m.prefix with
                    | '%' ->
                        if pulse.value = Low then
                            m.state <- not m.state
                            if m.state then High else Low
                        else
                            proceed <- false
                            Low
                    | '&' ->
                        m.memory.[pulse.fromName] <- pulse.value
                        let isHighForAll = m.memory.Values |> Seq.forall (fun v -> v = High)
                        if isHighForAll then Low else High
                    | _ -> pulse.value
                if proceed then
                    for destName in m.destinations do
                        let newPulse = { value = newPulseValue; fromName = pulse.toName; toName = destName }
                        pulseQueue.Add(newPulse)
                else
                    ()
    (cntLow, cntHigh)

let solve (input: string[]) =
    let startPulse = { value = Low; fromName = "button"; toName = "broadcaster" }
    let numCycle = 1000
    let modules = parseInput input
    let (low, high) = pushButton modules startPulse numCycle
    low * high

let readFile (fileName: string) = File.ReadAllLines(fileName)

[<EntryPoint>]
let main argv =
    let input = readFile "input.txt"
    let result = solve input
    printfn "%d" result
    0
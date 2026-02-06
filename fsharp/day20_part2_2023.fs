
open System
open System.Collections.Generic
open System.IO

type Pulse = Low | High
type ModuleType = B | FF | CJ
type ModDef = { Type: ModuleType; Name: string; Outputs: string list }

let rec gcd a b = if b = 0L then a else gcd b (a % b)
let lcm a b = if a = 0L || b = 0L then 0L else abs (a * b) / (gcd a b)

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let defs = lines |> Array.map (fun l ->
        let parts = l.Split([| " -> " |], StringSplitOptions.RemoveEmptyEntries)
        let namePart = parts.[0]
        let outputs = parts.[1].Split([| ", " |], StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun s -> s.Trim()) |> Array.toList
        if namePart = "broadcaster" then { Type = B; Name = "broadcaster"; Outputs = outputs }
        elif namePart.StartsWith("%") then { Type = FF; Name = namePart.Substring(1); Outputs = outputs }
        else { Type = CJ; Name = namePart.Substring(1); Outputs = outputs }
    )

    let modMap = defs |> Array.map (fun m -> m.Name, m) |> dict
    let ffStates = Dictionary<string, bool>()
    let cjStates = Dictionary<string, Dictionary<string, Pulse>>()
    
    for m in defs do
        if m.Type = FF then ffStates.[m.Name] <- false
        for out in m.Outputs do
            if modMap.ContainsKey(out) && modMap.[out].Type = CJ then
                if not (cjStates.ContainsKey(out)) then cjStates.[out] <- Dictionary<string, Pulse>()
                cjStates.[out].[m.Name] <- Low

    let rxFeeder = defs |> Array.find (fun m -> List.contains "rx" m.Outputs)
    let cycleNodes = cjStates.[rxFeeder.Name].Keys |> Seq.toList
    let loopLengths = Dictionary<string, int64>()
    
    let mutable presses = 0L
    let queue = Queue<string * string * Pulse>()

    while loopLengths.Count < cycleNodes.Length && presses < 1000000L do
        presses <- presses + 1L
        queue.Enqueue("button", "broadcaster", Low)
        while queue.Count > 0 do
            let src, target, pulse = queue.Dequeue()
            
            if target = rxFeeder.Name && pulse = High && List.contains src cycleNodes then
                if not (loopLengths.ContainsKey(src)) then loopLengths.[src] <- presses

            if modMap.ContainsKey(target) then
                let m = modMap.[target]
                match m.Type with
                | B ->
                    for out in m.Outputs do queue.Enqueue(target, out, pulse)
                | FF ->
                    if pulse = Low then
                        let state = not (if ffStates.ContainsKey(target) then ffStates.[target] else false)
                        ffStates.[target] <- state
                        let send = if state then High else Low
                        for out in m.Outputs do queue.Enqueue(target, out, send)
                | CJ ->
                    cjStates.[target].[src] <- pulse
                    let send = if Seq.forall (fun (p: Pulse) -> p = High) (cjStates.[target].Values) then Low else High
                    for out in m.Outputs do queue.Enqueue(target, out, send)

    if loopLengths.Count = cycleNodes.Length then
        let ans = Seq.fold lcm 1L loopLengths.Values
        printfn "%d" ans
    else
        printfn "0"
    0


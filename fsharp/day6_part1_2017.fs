
open System
open System.IO

[<EntryPoint>]
let main _ =
    let banks =
        File.ReadAllText("input.txt")
            .Split([|' '; '\t'; '\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int
    let bankCount = banks.Length
    let mutable current = Array.copy banks
    let mutable seen = Set.empty<string>
    let mutable cycles = 0
    let mutable running = true
    while running do
        let key = String.concat "," (Array.map string current)
        if Set.contains key seen then
            running <- false
        else
            seen <- Set.add key seen
            let maxIdx =
                current
                |> Array.mapi (fun i v -> i, v)
                |> Array.maxBy snd
                |> fst
            let mutable blocks = current.[maxIdx]
            current.[maxIdx] <- 0
            for i = 1 to blocks do
                let idx = (maxIdx + i) % bankCount
                current.[idx] <- current.[idx] + 1
            cycles <- cycles + 1
    printfn "It takes %d redistribution cycles to reach a repeated configuration." cycles
    0

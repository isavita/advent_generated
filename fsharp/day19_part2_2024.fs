
open System
open System.IO

let trim (s:string) = s.Trim()

let countWays (design:string) (patterns:string[]) =
    let n = design.Length
    let dp = Array.zeroCreate (n+1)
    dp.[0] <- 1L
    for i in 1..n do
        for p in patterns do
            let lp = p.Length
            if lp <= i && design.Substring(i-lp,lp) = p then
                dp.[i] <- dp.[i] + dp.[i-lp]
    dp.[n]

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines("input.txt")
    let patterns = lines.[0].Split(',') |> Array.map trim
    use rd = new StringReader(lines.[2..] |> String.concat "\n")
    let mutable total = 0L
    let mutable line = rd.ReadLine()
    while not (isNull line) do
        let design = trim line
        if design <> "" then total <- total + countWays design patterns
        line <- rd.ReadLine()
    printfn "%d" total
    0

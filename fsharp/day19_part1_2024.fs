
open System
open System.IO

let readLines (path:string) = File.ReadAllLines path

let trim (s:string) = s.Trim()

let canMake (design:string) (patterns:string[]) =
    let n = design.Length
    let dp = Array.zeroCreate (n+1)
    dp.[0] <- true
    for i in 1..n do
        for pat in patterns do
            let lp = pat.Length
            if lp <= i && dp.[i-lp] && design.Substring(i-lp, lp) = pat then
                dp.[i] <- true
    dp.[n]

[<EntryPoint>]
let main _ =
    let lines = readLines "input.txt"
    let patterns = lines.[0].Split(',') |> Array.map trim
    let mutable count = 0
    for i in 2..lines.Length-1 do
        let line = trim lines.[i]
        if line.Length > 0 && canMake line patterns then
            count <- count + 1
    printfn "%d" count
    0

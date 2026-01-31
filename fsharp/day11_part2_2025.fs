
open System
open System.IO
open System.Collections.Generic

let nodes = Dictionary<string,int>()
let nbr = ResizeArray<ResizeArray<int>>()

let idx (s:string) =
    match nodes.TryGetValue s with
    | true,i -> i
    | _ ->
        let i = nodes.Count
        nodes.Add(s,i)
        nbr.Add(ResizeArray())
        i

let addEdge u v = nbr.[u].Add v

let countPaths src tgt =
    let memo = Array.create nodes.Count -1L
    let rec dfs cur =
        if cur = tgt then 1L
        else
            match memo.[cur] with
            | -1L ->
                let mutable sum = 0L
                for v in nbr.[cur] do sum <- sum + dfs v
                memo.[cur] <- sum
                sum
            | x -> x
    dfs src

let lines = File.ReadAllLines "input.txt"
for line in lines do
    let colon = line.IndexOf ':'
    if colon >= 0 then
        let src = line.[..colon-1].Trim()
        let dst = line.[colon+1..].Trim()
        let u = idx src
        for tok in dst.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) do
            addEdge u (idx tok)

let svr = idx "svr"
let dac = idx "dac"
let fft = idx "fft"
let out = idx "out"

let s1 = countPaths svr dac * countPaths dac fft * countPaths fft out
let s2 = countPaths svr fft * countPaths fft dac * countPaths dac out

printfn "Paths (svr->dac->fft->out): %d" s1
printfn "Paths (svr->fft->dac->out): %d" s2
printfn "Total paths visiting both: %d" (s1+s2)

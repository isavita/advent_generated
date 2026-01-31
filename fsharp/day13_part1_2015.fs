
open System
open System.IO
open System.Collections.Generic

let mutable maxHappiness = Int32.MinValue

let rec permute (arr:int[]) (l:int) (r:int) (h:int[,]) =
    if l = r then
        let n = arr.Length
        let mutable sum = 0
        for i in 0 .. n-1 do
            let left = (i - 1 + n) % n
            let right = (i + 1) % n
            sum <- sum + h.[arr.[i], arr.[left]] + h.[arr.[i], arr.[right]]
        if sum > maxHappiness then maxHappiness <- sum
    else
        for i in l .. r do
            let tmp = arr.[l]
            arr.[l] <- arr.[i]
            arr.[i] <- tmp
            permute arr (l+1) r h
            arr.[i] <- arr.[l]
            arr.[l] <- tmp

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let idx = Dictionary<string,int>()
    let mutable next = 0
    let triples = ResizeArray<string * string * int>()
    for line in lines do
        let parts = line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
        let from = parts.[0]
        let act = parts.[2]
        let v = int parts.[3]
        let toName = parts.[10].TrimEnd('.')
        let value = if act = "lose" then -v else v
        triples.Add (from, toName, value)
        if not (idx.ContainsKey from) then idx.[from] <- next; next <- next + 1
        if not (idx.ContainsKey toName) then idx.[toName] <- next; next <- next + 1
    let n = next
    let happiness = Array2D.create n n 0
    for (f,t,v) in triples do
        let i = idx.[f]
        let j = idx.[t]
        happiness.[i,j] <- v
    let arr = Array.init n id
    permute arr 1 (n-1) happiness
    printfn "%d" maxHappiness
    0

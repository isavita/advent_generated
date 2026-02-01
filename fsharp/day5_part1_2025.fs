
open System
open System.IO
open System.Collections.Generic

let trim (s:string) = s.Trim()

let parseRange (s:string) =
    let parts = s.Split('-')
    let a = Int64.Parse(trim parts.[0])
    let b = Int64.Parse(trim parts.[1])
    (a, b)

let merge (arr:(int64*int64)[]) =
    let res = List<(int64*int64)>()
    for (a,b) in arr do
        if res.Count = 0 then res.Add(a,b)
        else
            let (pa,pb) = res.[res.Count-1]
            if a > pb then res.Add(a,b)
            elif b > pb then res.[res.Count-1] <- (pa,b)
    res.ToArray()

let rec contains (arr:(int64*int64)[]) x lo hi =
    if lo > hi then false
    else
        let mid = (lo + hi) >>> 1
        let (mn,mx) = arr.[mid]
        if x < mn then contains arr x lo (mid-1)
        elif x > mx then contains arr x (mid+1) hi
        else true

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines("input.txt")
    let sep = lines |> Array.findIndex (fun s -> String.IsNullOrWhiteSpace s)
    let rangeLines = lines.[0..sep-1]
    let idLines = lines.[sep+1..]

    let ranges =
        rangeLines
        |> Array.map parseRange
        |> Array.sortBy (fun (a,_) -> a)
        |> merge

    let mutable fresh = 0L
    for line in idLines do
        if not (String.IsNullOrWhiteSpace line) then
            let id = Int64.Parse(trim line)
            if ranges.Length > 0 && contains ranges id 0 (ranges.Length-1) then
                fresh <- fresh + 1L

    printfn "Number of fresh ingredients: %d" fresh
    0

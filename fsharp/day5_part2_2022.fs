
open System
open System.IO

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let stacks = ResizeArray<string>()
    let mutable i = 0
    while not (lines.[i].[1] = '1') do
        stacks.Add lines.[i]
        i <- i + 1
    let n = (lines.[i].Split ' ' |> Array.filter ((<>) "") |> Array.last |> int)
    i <- i + 2
    let arr = Array.init n (fun _ -> ResizeArray<char>())
    for l in stacks |> Seq.rev do
        for j in 0 .. n-1 do
            let c = l.[1 + j * 4]
            if Char.IsLetter c then arr.[j].Add c
    while i < lines.Length do
        let p = lines.[i].Split ' '
        let a, f, t = int p.[1], int p.[3] - 1, int p.[5] - 1
        let temp = arr.[f].GetRange(arr.[f].Count - a, a)
        arr.[f].RemoveRange(arr.[f].Count - a, a)
        arr.[t].AddRange(temp)
        i <- i + 1
    arr |> Seq.iter (fun s -> printf "%c" s.[s.Count - 1])
    0

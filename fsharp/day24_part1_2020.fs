
open System
open System.IO
open System.Collections.Generic

let dirs = dict [
    "e",  (1, 0)
    "se", (0, 1)
    "sw", (-1, 1)
    "w",  (-1, 0)
    "nw", (0, -1)
    "ne", (1, -1)
]

[<EntryPoint>]
let main _ =
    let black = HashSet<(int * int)>()
    use sr = new StreamReader("input.txt")
    let mutable line = sr.ReadLine()
    while not (isNull line) do
        let mutable q, r = 0, 0
        let mutable i = 0
        while i < line.Length do
            let dir =
                match line.[i] with
                | 'e' | 'w' -> string line.[i]
                | 'n' | 's' ->
                    let d = line.Substring(i, 2)
                    i <- i + 1
                    d
                | _ -> ""
            let dq, dr = dirs.[dir]
            q <- q + dq
            r <- r + dr
            i <- i + 1
        let coord = (q, r)
        if black.Contains(coord) then black.Remove(coord) |> ignore
        else black.Add(coord) |> ignore
        line <- sr.ReadLine()
    printfn "%d" black.Count
    0

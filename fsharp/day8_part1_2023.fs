
open System
open System.IO

let [<Literal>] MAX_NODES = 17576
let [<Literal>] MAX_INSTRUCTIONS = 512

let inline nameToIndex (s: string) =
    (int s.[0] - int 'A') * 676 + (int s.[1] - int 'A') * 26 + (int s.[2] - int 'A')

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines("input.txt")
    let instructions = lines.[0].ToCharArray()
    let instrLen = instructions.Length
    let map = Array.init MAX_NODES (fun _ -> -1, -1)
    for i = 2 to lines.Length - 1 do
        let line = lines.[i]
        if line.Length >= 10 then
            let node = nameToIndex(line.Substring(0, 3))
            let left = nameToIndex(line.Substring(7, 3))
            let right = nameToIndex(line.Substring(12, 3))
            map.[node] <- (left, right)

    let mutable current = nameToIndex("AAA")
    let target = nameToIndex("ZZZ")
    let mutable steps = 0L
    while current <> target do
        let dir = instructions.[int (steps % int64 instrLen)]
        let left, right = map.[current]
        current <- if dir = 'L' then left else right
        steps <- steps + 1L
    printfn "%d" steps
    0


module DayN

open System
open System.IO
open System.Text.RegularExpressions

type Node = { used: int; avail: int }

let readNodes (filename: string) =
    use file = File.OpenText filename
    let nodes = ref []
    let nodeRegex = new Regex("node-x\\d+-y\\d+\\s+\\d+T\\s+(\\d+)T\\s+(\\d+)T\\s+\\d+%")
    while not file.EndOfStream do
        let line = file.ReadLine()
        let matches = nodeRegex.Match(line)
        if matches.Success then
            let used = Int32.Parse(matches.Groups.[1].Value)
            let avail = Int32.Parse(matches.Groups.[2].Value)
            nodes := { used = used; avail = avail } :: !nodes
    !nodes |> List.rev

let countViablePairs (nodes: Node list) =
    let mutable count = 0
    for i = 0 to List.length nodes - 1 do
        let a = List.nth nodes i
        for j = 0 to List.length nodes - 1 do
            let b = List.nth nodes j
            if i <> j && a.used > 0 && a.used <= b.avail then
                count <- count + 1
    count

let nodes = readNodes "input.txt"
let viablePairs = countViablePairs nodes
printfn "%d" viablePairs

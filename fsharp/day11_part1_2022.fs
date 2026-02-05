open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

type Op =
    | Add of uint64 option
    | Multiply of uint64 option
    | Square

type Monkey =
    {
        mutable items: Queue<uint64>
        op: Op
        testDiv: int
        trueMonkey: int
        falseMonkey: int
        mutable inspections: uint64
    }

let parseOp (s: string) : Op =
    let body = s.Substring(s.IndexOf('=') + 1).Trim()
    if body.Contains("old * old") then Square
    elif body.Contains("old *") then
        let idx = body.IndexOf('*')
        let rest = body.Substring(idx + 1).Trim()
        let v = UInt64.Parse(rest)
        Multiply (Some v)
    elif body.Contains("old + old") then Add None
    elif body.Contains("old +") then
        let idx = body.IndexOf('+')
        let rest = body.Substring(idx + 1).Trim()
        let v = UInt64.Parse(rest)
        Add (Some v)
    else
        failwith "Unknown operation"

let queueFromList (xs: uint64 list) =
    let q = Queue<uint64>()
    xs |> List.iter (fun x -> q.Enqueue(x))
    q

let readMonkeys (lines: string[]) =
    let monkeys = ResizeArray<Monkey>()
    let mutable i = 0
    while i < lines.Length do
        let line = lines.[i]
        if line.StartsWith("Monkey") then
            if i + 5 < lines.Length then
                let itemsLine = lines.[i + 1]
                let opLine = lines.[i + 2]
                let testLine = lines.[i + 3]
                let trueLine = lines.[i + 4]
                let falseLine = lines.[i + 5]

                let itemsPartIndex = itemsLine.IndexOf(':')
                let itemsPart = if itemsPartIndex >= 0 then itemsLine.Substring(itemsPartIndex + 1).Trim() else ""
                let itemList =
                    if itemsPart = "" then []
                    else itemsPart.Split(',') |> Array.map (fun s -> UInt64.Parse(s.Trim())) |> Array.toList

                let op = parseOp opLine

                let testMatch = Regex.Match(testLine, @"\d+")
                let testDiv = int testMatch.Value

                let trueMatch = Regex.Match(trueLine, @"\d+")
                let falseMatch = Regex.Match(falseLine, @"\d+")
                let trueMonkey = int trueMatch.Value
                let falseMonkey = int falseMatch.Value

                let monkey = 
                    {
                        items = queueFromList itemList
                        op = op
                        testDiv = testDiv
                        trueMonkey = trueMonkey
                        falseMonkey = falseMonkey
                        inspections = 0UL
                    }

                monkeys.Add(monkey)
                i <- i + 6
            else
                i <- i + 1
        else
            i <- i + 1
    monkeys

[<EntryPoint>]
let main argv =
    let lines = File.ReadAllLines("input.txt")
    let monkeys = readMonkeys lines
    let arr = monkeys.ToArray()
    let rounds = 20
    for _ in 0 .. rounds - 1 do
        for idx in 0 .. arr.Length - 1 do
            let m = arr.[idx]
            let count = m.items.Count
            m.inspections <- m.inspections + uint64 count
            for _ in 0 .. count - 1 do
                let item = m.items.Dequeue()
                let moved =
                    match m.op with
                    | Add opt -> match opt with Some v -> item + v | None -> item + item
                    | Multiply opt -> match opt with Some v -> item * v | None -> item * item
                    | Square -> item * item
                let after = moved / 3UL
                let target = if after % uint64 m.testDiv = 0UL then m.trueMonkey else m.falseMonkey
                arr.[target].items.Enqueue(after)
    let counts = arr |> Array.map (fun m -> m.inspections) |> Array.toList
    let sorted = List.sortDescending counts
    let result =
        match sorted with
        | a :: b :: _ -> a * b
        | [a] -> a
        | [] -> 0UL
    printfn "%A" result
    0
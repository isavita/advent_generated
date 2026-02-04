open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

let gcd a b =
    let rec loop a b =
        if b = 0L then a else loop b (a % b)
    loop a b

let lcm a b = a / gcd a b * b

let lcmList lst =
    List.fold (fun acc x -> lcm acc x) 1L lst

let parseInput (lines : string[]) =
    let instructions = lines.[0]
    let nodeRegex = Regex(@"(\w+) = \((\w+), (\w+)\)")
    let nodes = Dictionary<string, string * string>()
    for i = 2 to lines.Length - 1 do
        let line = lines.[i]
        let m = nodeRegex.Match(line)
        if m.Success then
            let key = m.Groups.[1].Value
            let left = m.Groups.[2].Value
            let right = m.Groups.[3].Value
            nodes.[key] <- (left, right)
    instructions, nodes

let solve lines =
    let instructions, nodes = parseInput lines
    let instrLen = instructions.Length
    let starts =
        nodes.Keys
        |> Seq.filter (fun k -> k.EndsWith("A"))
        |> Seq.toList
    let steps =
        starts
        |> List.map (fun start ->
            let mutable node = start
            let mutable step = 0L
            while not (node.EndsWith("Z")) do
                let instr = instructions.[int (step % int64 instrLen)]
                node <-
                    if instr = 'L' then
                        fst (nodes.[node])
                    else
                        snd (nodes.[node])
                step <- step + 1L
            step)
    lcmList steps

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines("input.txt")
    let result = solve lines
    printfn "%d" result
    0

open System
open System.Collections.Generic
open System.IO

let pairings = 
    dict ['(', ')'; '[', ']'; '{', '}'; '<', '>']

let scores = 
    dict [')', 3; ']', 57; '}', 1197; '>', 25137]

let checkLine (line: string) =
    let stack = new Stack<char>()
    let rec check (chars: char list) =
        match chars with
        | [] -> (0, false)
        | c :: cs ->
            if pairings.ContainsKey c then
                stack.Push(pairings[c])
                check cs
            else
                if stack.Count = 0 || stack.Peek() <> c then
                    (scores[c], true)
                else
                    stack.Pop() |> ignore
                    check cs
    check (List.ofSeq line)

let main () =
    try
        File.ReadAllLines("input.txt")
        |> Seq.map checkLine
        |> Seq.filter snd
        |> Seq.sumBy fst
        |> printfn "%d"
    with
    | :? FileNotFoundException -> printfn "File 'input.txt' not found."

main ()

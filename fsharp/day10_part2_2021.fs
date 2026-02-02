
open System
open System.IO
open System.Linq

let getClosingCharValue = function
    | '(' -> 1L
    | '[' -> 2L
    | '{' -> 3L
    | '<' -> 4L
    | _ -> 0L

let checkAndCompleteLine (line: string) =
    let rec aux stack score =
        match stack with
        | [] -> None
        | head :: tail ->
            match line.[head] with
            | '(' | '[' | '{' | '<' as c ->
                aux (head + 1 :: tail) (score * 5L + getClosingCharValue c)
            | ')' | ']' | '}' | '>' ->
                if tail = [] then None
                else
                    let top = List.head tail
                    let topChar = line.[top]
                    if (line.[head] = ')' && topChar <> '(') || (line.[head] = ']' && topChar <> '[') ||
                       (line.[head] = '}' && topChar <> '{') || (line.[head] = '>' && topChar <> '<') then
                        None
                    else aux tail score
    let indices = seq { 0 .. line.Length - 1 } |> Seq.toList
    let result = aux indices 0L
    match result with
    | Some score -> Some score
    | None ->
        let mutable stack = []
        let mutable isCorrupted = false
        for i = 0 to line.Length - 1 do
            match line.[i] with
            | '(' | '[' | '{' | '<' -> stack <- line.[i] :: stack
            | ')' | ']' | '}' | '>' as c ->
                if stack = [] then isCorrupted <- true
                else
                    let top = List.head stack
                    stack <- List.tail stack
                    if (c = ')' && top <> '(') || (c = ']' && top <> '[') ||
                       (c = '}' && top <> '{') || (c = '>' && top <> '<') then
                        isCorrupted <- true
        if isCorrupted then None
        else
            let score =
                List.fold (fun acc c -> acc * 5L + getClosingCharValue c) 0L stack
            Some score

let quicksort (arr: int64 array) =
    let rec qsort low high =
        if low < high then
            let pi = partition low high
            qsort low (pi - 1)
            qsort (pi + 1) high

    and partition low high =
        let pivot = arr.[high]
        let mutable i = low - 1
        for j = low to high - 1 do
            if arr.[j] < pivot then
                i <- i + 1
                let temp = arr.[i]
                arr.[i] <- arr.[j]
                arr.[j] <- temp
        let temp = arr.[i + 1]
        arr.[i + 1] <- arr.[high]
        arr.[high] <- temp
        i + 1

    qsort 0 (arr.Length - 1)

[<EntryPoint>]
let main argv =
    try
        let scores =
            File.ReadAllLines("input.txt")
            |> Seq.map checkAndCompleteLine
            |> Seq.choose id
            |> Seq.toArray

        quicksort scores

        printfn "%d" scores.[scores.Length / 2]
        0
    with
    | ex -> printfn "%s" ex.Message; 1

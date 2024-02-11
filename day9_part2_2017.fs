
module Day9

let input = System.IO.File.ReadAllText "input.txt"

let rec processGroup (input: string) (score: int) (totalScore: int) (garbageCount: int) (inGarbage: bool) =
    match input with
    | "" -> (totalScore, garbageCount)
    | _ ->
        match input.[0] with
        | '{' when not inGarbage -> processGroup (input.[1..]) (score + 1) totalScore garbageCount false
        | '}' when not inGarbage -> processGroup (input.[1..]) (score - 1) (totalScore + score) garbageCount false
        | '<' when not inGarbage -> processGroup (input.[1..]) score totalScore garbageCount true
        | '>' when inGarbage -> processGroup (input.[1..]) score totalScore garbageCount false
        | '!' -> processGroup (input.[2..]) score totalScore garbageCount inGarbage
        | _ when inGarbage -> processGroup (input.[1..]) score totalScore (garbageCount + 1) inGarbage
        | _ -> processGroup (input.[1..]) score totalScore garbageCount inGarbage

let (_, part2) = processGroup input 1 0 0 false

printfn "%d" part2

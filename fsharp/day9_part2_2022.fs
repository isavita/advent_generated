
open System
open System.IO
open System.Collections.Generic

let sign (n: int) = 
    match n with
    | 0 -> 0
    | n when n < 0 -> -1
    | _ -> 1

let next (head: int[]) (tail: int[]) =
    if abs (head.[0] - tail.[0]) <= 1 && abs (head.[1] - tail.[1]) <= 1 then 
        tail
    else 
        let dx = sign (head.[0] - tail.[0])
        let dy = sign (head.[1] - tail.[1])
        [| tail.[0] + dx; tail.[1] + dy |]

let visited (input: string) ropeLen =
    let rope = Array.init ropeLen (fun _ -> [|0; 0|])
    let visited = new HashSet<(int * int)>()
    input.Split('\n')
    |> Seq.iter (fun line ->
        let parts = line.Split(' ')
        let dir = parts.[0].[0]
        let n = int parts.[1]
        let dx, dy = 
            match dir with
            | 'N' | 'U' | '^' -> 0, 1
            | 'S' | 'D' | 'v' -> 0, -1
            | 'E' | 'R' | '>' -> 1, 0
            | 'W' | 'L' | '<' -> -1, 0
            | _ -> failwith "Invalid direction"
        for _ in 1 .. n do
            rope.[0].[0] <- rope.[0].[0] + dx
            rope.[0].[1] <- rope.[0].[1] + dy
            for j in 1 .. ropeLen - 1 do
                rope.[j] <- next rope.[j - 1] rope.[j]
            visited.Add(rope.[ropeLen - 1].[0], rope.[ropeLen - 1].[1]) |> ignore)
    visited.Count

let main() =
    try
        let input = File.ReadAllText("input.txt").Trim()
        printfn "%d" (visited input 10)
    with
    | ex -> printfn "An error occurred: %s" ex.Message

main()

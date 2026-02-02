
open System
open System.IO
open System.Collections.Generic

type Point = { x: int; y: int }

let getPointsWithSteps (path: string) =
    let points = new Dictionary<Point, int>()
    let mutable current = { x = 0; y = 0 }
    let mutable steps = 0
    path.Split(',')
    |> Array.iter (fun token ->
        let dir = token.[0]
        let dist = int (token.Substring(1))
        for _ in 1 .. dist do
            steps <- steps + 1
            match dir with
            | 'U' -> current <- { current with y = current.y + 1 }
            | 'D' -> current <- { current with y = current.y - 1 }
            | 'L' -> current <- { current with x = current.x - 1 }
            | 'R' -> current <- { current with x = current.x + 1 }
            | _ -> failwith "Invalid direction"
            if not (points.ContainsKey current) then
                points.Add(current, steps)
    )
    points

let main() =
    try
        let lines = File.ReadAllLines("input.txt")
        if lines.Length < 2 then
            failwith "Invalid input file"
        let wire1 = getPointsWithSteps lines.[0]
        let wire2 = getPointsWithSteps lines.[1]
        let minSteps =
            wire1
            |> Seq.filter (fun kvp -> wire2.ContainsKey kvp.Key)
            |> Seq.map (fun kvp -> kvp.Value + wire2.[kvp.Key])
            |> Seq.min
        printfn "%d" minSteps
    with
    | ex -> printfn "Error: %s" ex.Message

main()

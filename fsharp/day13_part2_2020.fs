
open System
open System.IO

let readLines (filePath:string) = File.ReadAllLines(filePath)

let main =
    try
        let lines = readLines "input.txt"
        if lines.Length < 2 then
            printfn "Invalid input"
        else
            let timestamp = int lines.[0]
            let buses = lines.[1].Split(',')
            
            // Part 1
            let busIds = buses |> Array.choose (function | "x" -> None | id -> Some(int id))
            let minWait = busIds |> Array.map (fun id -> id - timestamp % id) |> Array.min
            let busId = busIds |> Array.find (fun id -> id - timestamp % id = minWait)
            printfn "Part 1: %d" (busId * minWait)
            
            // Part 2
            let rec findTimestamp timestamp step i =
                if i >= buses.Length then timestamp
                else
                    match buses.[i] with
                    | "x" -> findTimestamp timestamp step (i + 1)
                    | id ->
                        let id = int id
                        let newTimestamp = 
                            Seq.initInfinite (fun x -> timestamp + int64 x * step)
                            |> Seq.find (fun t -> (t + int64 i) % int64 id = 0L)
                        findTimestamp newTimestamp (step * int64 id) (i + 1)
            let timestamp2 = findTimestamp 0L 1L 0
            printfn "Part 2: %d" timestamp2
    with
    | :? FileNotFoundException -> printfn "File not found"

[<EntryPoint>]
let entryPoint argv =
    main
    0


open System
open System.IO
open System.Linq

let calculateNewFuel (currentPosition: int) (newPosition: int) : int =
    let diff = abs (currentPosition - newPosition)
    (diff * (diff + 1)) / 2

let main =
    try
        let input = File.ReadAllText("input.txt").Split(',') |> Array.map int
        let minPos = Array.min input
        let maxPos = Array.max input
        let minFuel = 
            seq { minPos .. maxPos } 
            |> Seq.map (fun pos -> input |> Array.sumBy (fun x -> calculateNewFuel x pos)) 
            |> Seq.min
        printfn "%d" minFuel
    with
    | ex -> printfn "Error: %s" ex.Message

[<EntryPoint>]
let entryPoint argv =
    main
    0

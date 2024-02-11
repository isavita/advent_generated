
module Day13

let input = System.IO.File.ReadAllLines "input.txt"
let earliestTimestamp = int input.[0]
let busIDs = input.[1].Split(',') |> Array.filter (fun x -> x <> "x") |> Array.map int

let findEarliestBus () =
    let mutable minWaitTime = System.Int32.MaxValue
    let mutable earliestBusID = 0
    for id in busIDs do
        let waitTime = id - (earliestTimestamp % id)
        if waitTime < minWaitTime then
            minWaitTime <- waitTime
            earliestBusID <- id
    minWaitTime * earliestBusID

printfn "%d" (findEarliestBus ())

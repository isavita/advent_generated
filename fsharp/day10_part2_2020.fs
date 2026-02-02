
open System
open System.IO
open System.Linq

let countArrangements (adapters: int array) =
    let ways = Array.init (Array.length adapters) (fun _ -> 0L)
    ways.[0] <- 1L

    for i = 1 to adapters.Length - 1 do
        for j = 1 to 3 do
            if i - j >= 0 && adapters.[i] - adapters.[i - j] <= 3 then
                ways.[i] <- ways.[i] + ways.[i - j]

    ways.[adapters.Length - 1]

let main () =
    try
        let adapters =
            File.ReadAllLines("input.txt")
            |> Array.map int
            |> fun xs -> Array.append [|0|] xs
            |> Array.sort
            |> fun xs -> Array.append xs [|xs.[xs.Length - 1] + 3|]

        printfn "%d" (countArrangements adapters)
    with
    | :? FileNotFoundException -> printfn "Error opening file"

main ()

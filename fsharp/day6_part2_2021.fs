
open System
open System.IO
open System.Linq

[<EntryPoint>]
let main argv =
    let counts = Array.init 9 (fun _ -> 0L)
    File.ReadAllText("input.txt").Split([|','|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.iter (fun s -> counts.[int s] <- counts.[int s] + 1L)

    for day in 0 .. 255 do
        let spawn = counts.[0]
        Array.Copy(counts, 1, counts, 0, 8)
        counts.[6] <- counts.[6] + spawn
        counts.[8] <- spawn

    printfn "%d" (Array.sum counts)
    0

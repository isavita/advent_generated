
open System
open System.IO
open System.Linq

let countFish (fishes: int64 array) =
    for day = 1 to 80 do
        let newFish = fishes.[0]
        for i = 1 to fishes.Length - 1 do
            fishes.[i - 1] <- fishes.[i]
        fishes.[6] <- fishes.[6] + newFish
        fishes.[8] <- newFish
    Array.sum fishes

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText("input.txt")
    let fishes = Array.init 9 (fun _ -> 0L)
    input.Split(',') 
    |> Array.iter (fun x -> let fish = int64 x in fishes.[int fish] <- fishes.[int fish] + 1L)
    let totalFish = countFish fishes
    printfn "%d" totalFish
    0

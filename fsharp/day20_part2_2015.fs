
open System.IO

[<EntryPoint>]
let main _ =
    let target = (File.ReadAllText "input.txt" |> int) / 11

    let houses = Array.zeroCreate (target + 1)

    for elf in 1..target do
        let maxHouse = min (elf * 50) target
        for house in elf..elf..maxHouse do
            houses.[house] <- houses.[house] + elf

    printfn "%d" (Array.findIndex (fun v -> v >= target) houses)
    0


open System.IO

[<EntryPoint>]
let main _ =
    let reg = ResizeArray<int>([|1|])
    File.ReadAllLines "input.txt"
    |> Array.iter (fun l ->
        if l = "noop" then reg.Add(reg.[reg.Count-1])
        else reg.Add(reg.[reg.Count-1]); reg.Add(reg.[reg.Count-1] + int(l.[5..])))

    for y in 0..5 do
        for x in 0..39 do
            printf "%s" (if abs(x - reg.[y*40+x]) <= 1 then "#" else ".")
        printfn ""
    0

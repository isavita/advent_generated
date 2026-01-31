
open System.IO

[<EntryPoint>]
let main _ =
    let x = ResizeArray<int>([|1|])
    File.ReadAllLines "input.txt"
    |> Array.iter (fun l ->
        let last = x.[x.Count-1]
        if l.StartsWith "noop" then x.Add last
        else
            let n = int(l.[5..])
            x.Add last
            x.Add(last + n))

    let mutable sum = 0
    for i in 19 .. 40 .. (x.Count-1) do
        sum <- sum + (i+1)*x.[i]
    printfn "%d" sum
    0

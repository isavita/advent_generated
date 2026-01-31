
open System.IO

let readAll (path: string) = File.ReadAllText path

let firstNUnique (n: int) (s: string) =
    let rec loop i =
        if i >= n then
            let set = s.Substring(i - n, n) |> Set.ofSeq
            if Set.count set = n then i
            else loop (i + 1)
        else -1
    loop n

[<EntryPoint>]
let main _ =
    printfn "%d" (readAll "input.txt" |> firstNUnique 4)
    0

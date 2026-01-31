
open System
open System.IO

let isPrime n =
    if n < 2 then false
    else
        let limit = int (Math.Sqrt(float n))
        let rec loop i =
            if i > limit then true
            elif n % i = 0 then false
            else loop (i + 1)
        loop 2

[<EntryPoint>]
let main _ =
    File.ReadAllText("input.txt") |> ignore
    let b = 57 * 100 + 100000
    let c = b + 17000
    let h =
        seq { b .. 17 .. c }
        |> Seq.filter (fun x -> not (isPrime x))
        |> Seq.length
    printfn "%d" h
    0

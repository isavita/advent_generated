
open System
open System.IO

let priority (c: char) =
    if c >= 'a' && c <= 'z' then int c - int 'a' + 1
    else int c - int 'A' + 27

[<EntryPoint>]
let main _ =
    File.ReadAllLines "input.txt"
    |> Array.sumBy (fun (s: string) ->
        let n = s.Length / 2
        let a, b = s.Substring(0, n), s.Substring(n, n)
        let setA = set a
        let setB = set b
        setA |> Set.intersect setB |> Seq.head |> priority)
    |> printfn "%d"
    0

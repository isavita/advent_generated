
open System
open System.IO
open System.Text

let diffAndCommon (a:string) (b:string) =
    let sb = StringBuilder()
    let mutable diff = 0
    for i = 0 to a.Length - 1 do
        if a.[i] = b.[i] then sb.Append(a.[i]) |> ignore
        else diff <- diff + 1
    if diff = 1 then Some (sb.ToString()) else None

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines("input.txt")
    let n = lines.Length
    let answer =
        seq {
            for i = 0 to n - 2 do
                for j = i + 1 to n - 1 do
                    yield (lines.[i], lines.[j])
        }
        |> Seq.choose (fun (a, b) -> diffAndCommon a b)
        |> Seq.tryHead
    match answer with
    | Some s -> printfn "%s" s
    | None -> ()
    0

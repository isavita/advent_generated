
open System
open System.IO

let hash (s:string) =
    let mutable h = 0
    for c in s do
        h <- ((h + int c) * 17) % 256
    h

let parseStep (s:string) =
    let opIdx = max (s.IndexOf '=' ) (s.IndexOf '-' )
    let label = s.Substring(0, opIdx)
    let op = s.[opIdx]
    let num = if op = '=' then int (s.Substring(opIdx+1)) else 0
    (hash label, label, op, num)

[<EntryPoint>]
let main _ =
    let input = File.ReadAllText "input.txt" |> fun s -> s.TrimEnd('\n')
    let steps = input.Split ','

    let boxes = Array.init 256 (fun _ -> ResizeArray<string*int>())

    for _, label, op, num in steps |> Array.map parseStep do
        let box = boxes.[hash label]
        let idx = box.FindIndex(fun (l, _) -> l = label)
        match op with
        | '-' -> if idx >= 0 then box.RemoveAt idx
        | '=' -> if idx >= 0 then box.[idx] <- (label, num) else box.Add (label, num)
        | _ -> ()

    let power =
        boxes
        |> Array.mapi (fun i box ->
            box
            |> Seq.mapi (fun j (_, f) -> (i+1)*(j+1)*f)
            |> Seq.sum)
        |> Array.sum

    printfn "%d" power
    0


open System
open System.IO

let getIntFromRow (row: string) =
    row |> Seq.fold (fun acc c -> (acc <<< 1) + (if c = '#' then 1 else 0)) 0

let getColumns (mirror: string[]) =
    let w = mirror.[0].Length
    Array.init w (fun i -> mirror |> Array.map (fun r -> r.[i]) |> String.Concat)

let isMirror (values: int[]) axis =
    let len = values.Length
    let limit = min axis (len - axis)
    seq { 0 .. limit - 1 }
    |> Seq.forall (fun j -> values.[axis - 1 - j] = values.[axis + j])

let getMirrorAxis (values: int[]) =
    let rec loop i =
        if i >= values.Length then 0
        elif isMirror values i then i
        else loop (i + 1)
    loop 1

let getMirrorValue (mirror: string[]) =
    let rows = mirror |> Array.map getIntFromRow
    let cols = mirror |> getColumns |> Array.map getIntFromRow
    let rowMirror = getMirrorAxis rows
    let colMirror = getMirrorAxis cols
    colMirror + rowMirror * 100

[<EntryPoint>]
let main _ =
    let input = File.ReadAllText "input.txt"
    let mirrors =
        input.Split([| Environment.NewLine + Environment.NewLine |], StringSplitOptions.None)
        |> Array.map (fun m -> m.Split([| Environment.NewLine |], StringSplitOptions.None))
    let result = mirrors |> Array.sumBy getMirrorValue
    printfn "%d" result
    0

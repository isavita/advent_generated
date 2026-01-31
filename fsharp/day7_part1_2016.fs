
open System
open System.IO

let hasAbba (s:string) (start:int) (len:int) (inside:bool) =
    let mutable outside = false
    let mutable insideFlag = false
    let mutable i = start
    while i <= len - 4 do
        if s.[i] = '[' then outside <- true
        elif s.[i] = ']' then outside <- false
        elif s.[i] <> s.[i+1] && s.[i] = s.[i+3] && s.[i+1] = s.[i+2] then
            if outside then insideFlag <- true else outside <- true
        i <- i + 1
    outside && not insideFlag, insideFlag

let supportsTls (line:string) =
    let mutable inHyper = false
    let mutable abbaOutside = false
    let mutable abbaInside = false
    for i = 0 to line.Length - 4 do
        match line.[i] with
        | '[' -> inHyper <- true
        | ']' -> inHyper <- false
        | _ when line.[i] <> line.[i+1] && line.[i] = line.[i+3] && line.[i+1] = line.[i+2] ->
            if inHyper then abbaInside <- true else abbaOutside <- true
        | _ -> ()
    abbaOutside && not abbaInside

[<EntryPoint>]
let main _ =
    let count =
        File.ReadAllLines("input.txt")
        |> Array.filter supportsTls
        |> Array.length
    printfn "%d" count
    0

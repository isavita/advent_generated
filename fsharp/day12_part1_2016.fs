
open System
open System.IO

let loadInstructions file =
    try File.ReadAllLines file
    with e -> printfn "Error reading file: %s" e.Message; [||]

let inline getValue (registers: int[]) = function
    | "a" -> registers.[0]
    | "b" -> registers.[1]
    | "c" -> registers.[2]
    | "d" -> registers.[3]
    | s   -> int s

let execute (inst: string[]) =
    let reg = Array.zeroCreate<int> 4
    let n = inst.Length
    let rec loop i =
        if i < n then
            let parts = inst.[i].Split ' '
            match parts.[0] with
            | "cpy" -> reg.[(int parts.[2].[0] - int 'a')] <- getValue reg parts.[1]; loop (i+1)
            | "inc" -> reg.[(int parts.[1].[0] - int 'a')] <- reg.[(int parts.[1].[0] - int 'a')] + 1; loop (i+1)
            | "dec" -> reg.[(int parts.[1].[0] - int 'a')] <- reg.[(int parts.[1].[0] - int 'a')] - 1; loop (i+1)
            | "jnz" -> let v = getValue reg parts.[1]
                       if v <> 0 then loop (i + int parts.[2]) else loop (i+1)
            | _   -> loop (i+1)
        else reg.[0]
    loop 0

[<EntryPoint>]
let main _ =
    loadInstructions "input.txt" |> execute |> printfn "%d"
    0

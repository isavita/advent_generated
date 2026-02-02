
open System
open System.IO

let countDigits (line: string) =
    let output = line.Split('|') |> Array.tryLast
    match output with
    | Some output ->
        output.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun token -> token.Trim())
        |> Array.filter (fun token -> token.Length = 2 || token.Length = 4 || token.Length = 3 || token.Length = 7)
        |> Array.length
    | None -> 0

let main () =
    try
        File.ReadAllLines("input.txt")
        |> Array.sumBy countDigits
        |> printfn "%d"
    with
    | :? FileNotFoundException -> printfn "Unable to open file"

main ()

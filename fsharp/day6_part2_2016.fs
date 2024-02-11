
module Day6

let input = System.IO.File.ReadAllLines "input.txt"

let getMostCommonCharacter (messages: string[]) columnIndex =
    messages
    |> Array.map (fun message -> message.[columnIndex])
    |> Array.groupBy id
    |> Array.maxBy (fun (_, occurences) -> occurences.Length)
    |> fst

let getLeastCommonCharacter (messages: string[]) columnIndex =
    messages
    |> Array.map (fun message -> message.[columnIndex])
    |> Array.groupBy id
    |> Array.minBy (fun (_, occurences) -> occurences.Length)
    |> fst

let errorCorrectedMessage =
    [0..input.[0].Length - 1]
    |> List.map (fun i -> getMostCommonCharacter input i)
    |> Array.ofList
    |> System.String

let originalMessage =
    [0..input.[0].Length - 1]
    |> List.map (fun i -> getLeastCommonCharacter input i)
    |> Array.ofList
    |> System.String

printfn "%s" errorCorrectedMessage
printfn "%s" originalMessage

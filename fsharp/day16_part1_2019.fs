
open System
open System.IO

let abs x = if x < 0 then -x else x

let applyFFT (input: int[]) =
    let basePattern = [|0; 1; 0; -1|]
    let length = input.Length
    Array.init length (fun i ->
        let sum = Array.sumBy (fun (j, x) -> basePattern.[((j + 1) / (i + 1)) % 4] * x) (Array.mapi (fun j x -> (j, x)) input)
        abs (sum % 10)
    )

let main() =
    try
        let input = File.ReadAllText("input.txt").Trim()
        let digits = input.ToCharArray() |> Array.map (fun c -> int c - int '0')
        let mutable digits' = digits
        for _ = 0 to 99 do
            digits' <- applyFFT digits'
        printfn "%s" (String.Concat(Array.take 8 digits'))
    with
    | ex -> printfn "Error: %s" ex.Message

main()

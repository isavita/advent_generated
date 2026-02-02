
open System
open System.IO
open System.Linq

let repeatInput (input: string) times =
    Array.init (input.Length * times) (fun i -> int (input.[i % input.Length]) - int '0')

let main =
    let input = File.ReadAllText("input.txt").Trim()
    let repeatedInput = repeatInput input 10000
    let offset = int (input.Substring(0, 7))

    for phase = 0 to 99 do
        let mutable sum = 0
        for i = repeatedInput.Length - 1 downto offset do
            sum <- sum + repeatedInput.[i]
            repeatedInput.[i] <- sum % 10

    let result = Array.sub repeatedInput offset 8 |> Array.map (fun x -> string x)
    printfn "%s" (String.concat "" result)

main

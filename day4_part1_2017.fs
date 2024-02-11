module Day4HighEntropyPassphrases

open System
open System.IO

let isValidPassphrase (passphrase: string) : bool =
    let words = passphrase.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
    let uniqueWords = words |> Seq.distinct |> Seq.toArray
    words.Length = uniqueWords.Length

let countValidPassphrases () : int =
    let filename = "input.txt" // Hardcoded file name
    File.ReadLines(filename)
    |> Seq.filter isValidPassphrase
    |> Seq.length

let main args =
    let validCount = countValidPassphrases ()
    printfn "%d" validCount
    0

main ()

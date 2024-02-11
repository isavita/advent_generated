module Day4HighEntropyPassphrasesPartTwo

open System
open System.IO

let isValidPassphrase (passphrase: string) : bool =
    let sortChars (word: string) = 
        word.ToCharArray() 
        |> Array.sort 
        |> Array.map string 
        |> String.concat ""
    let words = passphrase.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
    let sortedWords = words |> Array.map sortChars
    let uniqueWords = sortedWords |> Seq.distinct |> Seq.toArray
    sortedWords.Length = uniqueWords.Length

let countValidPassphrases () : int =
    let filename = "input.txt" // Hardcoded file name
    File.ReadLines(filename)
    |> Seq.filter isValidPassphrase
    |> Seq.length

let validCount = countValidPassphrases ()
printfn "%d" validCount


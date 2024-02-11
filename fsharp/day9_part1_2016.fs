module Day9ExplosivesInCyberspace

open System
open System.IO
open System.Text.RegularExpressions

let decompressAndCalculateLength (input: string) : int =
    let rec decompress (index: int) (outputLength: int) : int =
        if index >= input.Length then 
            outputLength
        else
            let remainingInput = input.Substring(index)
            let m = Regex.Match(remainingInput, @"\((\d+)x(\d+)\)")
            // Ensure match is used in a proper expression context
            match m.Success with
            | true when m.Index = 0 ->
                let length = int(m.Groups.[1].Value)
                let times = int(m.Groups.[2].Value)
                let startIndex = index + m.Index + m.Length
                let endIndex = startIndex + length
                decompress endIndex (outputLength + length * times)
            | _ ->
                decompress (index + 1) (outputLength + 1)
    decompress 0 0

let processCompressedData (filename: string) : int =
    let input = File.ReadAllText(filename).Replace(" ", "").Trim()
    decompressAndCalculateLength input

let main argv =
    let filename = "input.txt" // Ensure this path is correct for your input file location
    let decompressedLength = processCompressedData filename
    printfn "%d" decompressedLength
    0

main ()

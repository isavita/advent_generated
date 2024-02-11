module Day9ExplosivesInCyberspacePartTwo

open System
open System.IO
open System.Text.RegularExpressions

let rec calculateDecompressedLength (input: string) : int64 =
    let markerRegex = new Regex(@"\((\d+)x(\d+)\)")
    let mutable totalLength = 0L
    let mutable currentIndex = 0

    while currentIndex < input.Length do
        match markerRegex.Match(input, currentIndex) with
        | m when m.Success ->
            // Add the length of any text before the marker to the total
            totalLength <- totalLength + int64 (m.Index - currentIndex)

            // Extract the marker details
            let charsToRepeat = int(m.Groups.[1].Value)
            let repeatCount = int(m.Groups.[2].Value)
            let markerLength = m.Length

            // Calculate the length of the decompressed substring
            let decompressedSubstringLength = 
                calculateDecompressedLength (input.Substring(m.Index + markerLength, charsToRepeat))

            // Update the total length based on the decompressed substring length and the repeat count
            totalLength <- totalLength + decompressedSubstringLength * int64(repeatCount)

            // Move the current index past this marker and its decompressed substring
            currentIndex <- m.Index + markerLength + charsToRepeat
        | _ ->
            // If no more markers are found, add the length of the remaining text to the total
            totalLength <- totalLength + int64 (input.Length - currentIndex)
            currentIndex <- input.Length

    totalLength

let processCompressedData (filename: string) : int64 =
    let input = File.ReadAllText(filename).Replace(" ", "").Trim()
    calculateDecompressedLength input

let main argv =
    let filename = "input.txt"
    let decompressedLength = processCompressedData filename
    printfn "%d" decompressedLength
    0

main ()

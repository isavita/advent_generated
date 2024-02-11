module Day9StreamProcessing

open System
open System.IO

let calculateScores (input: string) : int =
    let mutable score = 0
    let mutable depth = 0
    let mutable inGarbage = false
    let mutable ignoreNext = false

    for char in input do
        match char with
        | '!' when inGarbage -> ignoreNext <- not ignoreNext
        | _ when ignoreNext -> ignoreNext <- false
        | '>' -> inGarbage <- false
        | '<' when not inGarbage -> inGarbage <- true
        | '{' when not inGarbage -> depth <- depth + 1
        | '}' when not inGarbage ->
            score <- score + depth
            depth <- depth - 1
        | _ -> ()

    score

let processStream (filename: string) : int =
    let input = File.ReadAllText(filename).Trim()
    calculateScores input

let main argv =
    let filename = "input.txt" // Ensure this path is correct for your input file location
    let totalScore = processStream filename
    printfn "%d" totalScore
    0 // Return value for the main function

main ()

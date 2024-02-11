module Task

open System
open System.IO

let readInput (filename:string) =
    use file = File.OpenText filename
    file.ReadLine()

let rec lookAndSay (sequence:string) iterations =
    let rec nextSequence (sequence:string) =
        let mutable result = System.Text.StringBuilder()
        let mutable i = 0
        while i < sequence.Length do
            let mutable count = 1
            let mutable digit = sequence.[i]
            let mutable j = i + 1
            while j < sequence.Length && sequence.[j] = digit do
                count <- count + 1
                j <- j + 1
            result.Append(count.ToString())
            result.Append(digit)
            i <- i + count
        result.ToString()

    let rec iterate (sequence:string) remainingIterations =
        if remainingIterations = 0 then sequence
        else iterate (nextSequence sequence) (remainingIterations - 1)

    iterate sequence iterations

let initialSequence = readInput "input.txt"
let result = lookAndSay initialSequence 40
printfn "%d" result.Length
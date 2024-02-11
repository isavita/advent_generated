
module Day8

let input = System.IO.File.ReadAllLines "input.txt"

let countCharactersDifference (input: string[]) =
    let mutable codeCount = 0
    let mutable memoryCount = 0
    let mutable encodedCount = 0

    for line in input do
        codeCount <- codeCount + line.Length
        memoryCount <- memoryCount + (System.Text.RegularExpressions.Regex.Unescape(line)).Length
        encodedCount <- encodedCount + (2 + line.Replace("\\", "\\\\").Replace("\"", "\\\"").Length)

    codeCount - memoryCount, encodedCount - codeCount

let _, resultPart2 = countCharactersDifference input

printfn "%d" resultPart2

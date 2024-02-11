module Day1

let input = System.IO.File.ReadAllText("input.txt")

let rec sumMatchingDigits (input:string) =
    let rec sumMatchingDigitsHelper (input:string) (acc:int) (index:int) =
        if index = input.Length then
            acc
        else
            let nextIndex = (index + 1) % input.Length
            if input.[index] = input.[nextIndex] then
                sumMatchingDigitsHelper input (acc + int(input.[index].ToString())) (index + 1)
            else
                sumMatchingDigitsHelper input acc (index + 1)
    
    sumMatchingDigitsHelper input 0 0

let resultPart1 = sumMatchingDigits input

let rec sumMatchingDigitsPart2 (input:string) =
    let rec sumMatchingDigitsPart2Helper (input:string) (acc:int) (index:int) (step:int) =
        if index = input.Length then
            acc
        else
            let nextIndex = (index + step) % input.Length
            if input.[index] = input.[nextIndex] then
                sumMatchingDigitsPart2Helper input (acc + int(input.[index].ToString())) (index + 1) step
            else
                sumMatchingDigitsPart2Helper input acc (index + 1) step
    
    sumMatchingDigitsPart2Helper input 0 0 (input.Length / 2)

let resultPart2 = sumMatchingDigitsPart2 input

printfn "%d" resultPart1
printfn "%d" resultPart2
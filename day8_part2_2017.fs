
module Day8

let input = System.IO.File.ReadAllLines "input.txt"

let mutable registers = System.Collections.Generic.Dictionary<string, int>()
let mutable highestValue = 0

for line in input do
    let parts = line.Split(' ')
    let register = parts.[0]
    let operation = parts.[1]
    let amount = int parts.[2]
    let conditionRegister = parts.[4]
    let conditionOperator = parts.[5]
    let conditionValue = int parts.[6]

    let mutable registerValue = 
        if registers.ContainsKey register then registers.[register] else 0

    let mutable conditionRegisterValue = 
        if registers.ContainsKey conditionRegister then registers.[conditionRegister] else 0

    let conditionMet = 
        match conditionOperator with
        | ">" -> conditionRegisterValue > conditionValue
        | "<" -> conditionRegisterValue < conditionValue
        | ">=" -> conditionRegisterValue >= conditionValue
        | "<=" -> conditionRegisterValue <= conditionValue
        | "==" -> conditionRegisterValue = conditionValue
        | "!=" -> conditionRegisterValue <> conditionValue
        | _ -> false

    if conditionMet then
        match operation with
        | "inc" -> registerValue <- registerValue + amount
        | "dec" -> registerValue <- registerValue - amount
        | _ -> ()

        registers.[register] <- registerValue

        if registerValue > highestValue then highestValue <- registerValue

printfn "%d" (registers.Values |> Seq.max)
printfn "%d" highestValue

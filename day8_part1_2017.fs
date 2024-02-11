
module DayN

open System

let file = System.IO.File.ReadAllLines "input.txt"

let mutable registers = System.Collections.Generic.Dictionary<string, int>()

for line in file do
    let parts = line.Split(' ')
    let reg = parts.[0]
    let op = parts.[1]
    let amount = Int32.Parse(parts.[2])
    let condReg = parts.[4]
    let condOp = parts.[5]
    let condVal = Int32.Parse(parts.[6])

    if not <| registers.ContainsKey reg then
        registers.Add(reg, 0)
    
    if not <| registers.ContainsKey condReg then
        registers.Add(condReg, 0)

    let mutable cond = false
    match condOp with
    | ">" -> cond <- registers.[condReg] > condVal
    | ">=" -> cond <- registers.[condReg] >= condVal
    | "<" -> cond <- registers.[condReg] < condVal
    | "<=" -> cond <- registers.[condReg] <= condVal
    | "==" -> cond <- registers.[condReg] = condVal
    | "!=" -> cond <- registers.[condReg] <> condVal

    if cond then
        match op with
        | "inc" -> registers.[reg] <- registers.[reg] + amount
        | "dec" -> registers.[reg] <- registers.[reg] - amount

let mutable maxValue = 0
for value in registers.Values do
    if value > maxValue then
        maxValue <- value

Console.WriteLine(maxValue)

module Day1

let input = System.IO.File.ReadAllLines("input.txt")
let mutable currentCalories = 0
let mutable maxCalories = 0

for line in input do
    if line = "" then
        if currentCalories > maxCalories then
            maxCalories <- currentCalories
        currentCalories <- 0
    else
        currentCalories <- currentCalories + int line

if currentCalories > maxCalories then
    maxCalories <- currentCalories

printfn "%d" maxCalories
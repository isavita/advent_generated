
module Day1

open System
open System.IO

let mutable numbers = []
let file = File.ReadAllLines("input.txt")
for line in file do
    if line <> "" then
        numbers <- numbers @ [Int32.Parse(line)]

for i = 0 to numbers.Length - 2 do
    for j = i + 1 to numbers.Length - 1 do
        if numbers.[i] + numbers.[j] = 2020 then
            Console.WriteLine(numbers.[i] * numbers.[j])
            Environment.Exit(0)

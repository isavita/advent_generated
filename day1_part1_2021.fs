
module Day1

open System.IO

let input = File.ReadAllLines "input.txt"
let mutable count = 0

for i = 1 to input.Length - 1 do
    if int input.[i] > int input.[i-1] then
        count <- count + 1

printfn "%d" count

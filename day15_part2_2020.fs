module Day15

let startingNumbers = System.IO.File.ReadAllText("input.txt").Trim().Split(",")
let spoken = System.Collections.Generic.Dictionary<int, int>()
let mutable lastSpoken = 0

for i = 0 to startingNumbers.Length - 1 do
    if i = startingNumbers.Length - 1 then
        lastSpoken <- int startingNumbers.[i]
    else
        spoken.[int startingNumbers.[i]] <- i + 1

for turn = startingNumbers.Length + 1 to 30000000 do
    let mutable nextNumber = 0
    if spoken.ContainsKey(lastSpoken) then
        nextNumber <- turn - 1 - spoken.[lastSpoken]
    spoken.[lastSpoken] <- turn - 1
    lastSpoken <- nextNumber

printfn "%d" lastSpoken
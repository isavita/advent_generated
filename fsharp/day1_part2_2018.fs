module DayXX

open System
open System.IO

let mutable frequencies = ResizeArray<int>()
let mutable currentFrequency = 0
frequencies.Add(currentFrequency)

let lines = File.ReadAllLines("input.txt")

while true do
    for line in lines do
        let frequencyDelta = Int32.Parse(line)
        currentFrequency <- currentFrequency + frequencyDelta
        if frequencies.Contains(currentFrequency) then
            Console.WriteLine(currentFrequency)
            Environment.Exit(0)
        frequencies.Add(currentFrequency)
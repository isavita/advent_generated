module Day8

open System

let rec calculateMemoryLength (s:string) =
    let mutable length = 0
    let mutable inEscape = false
    let mutable hexCount = 0

    for i = 1 to (s.Length - 2) do
        match () with
        | _ when hexCount > 0 -> hexCount <- hexCount - 1
        | _ when inEscape -> 
            if s.[i] = 'x' then
                hexCount <- 2
            inEscape <- false
            length <- length + 1
        | _ when s.[i] = '\\' -> inEscape <- true
        | _ -> length <- length + 1
    
    length

let file = System.IO.File.OpenText("input.txt")
let mutable totalDiff = 0

while not file.EndOfStream do
    let line = file.ReadLine()
    let codeLength = line.Length
    let memoryLength = calculateMemoryLength line
    totalDiff <- totalDiff + codeLength - memoryLength

Console.WriteLine(totalDiff)

open System
open System.IO

let input = int (File.ReadAllText "input.txt")

let scoreboard = System.Collections.Generic.List<int>([3; 7])
let mutable elf1 = 0
let mutable elf2 = 1

while scoreboard.Count < input + 10 do
    let s = scoreboard.[elf1] + scoreboard.[elf2]
    if s >= 10 then scoreboard.Add(s / 10)
    scoreboard.Add(s % 10)
    elf1 <- (elf1 + scoreboard.[elf1] + 1) % scoreboard.Count
    elf2 <- (elf2 + scoreboard.[elf2] + 1) % scoreboard.Count

for i in input .. input + 9 do
    printf "%d" scoreboard.[i]
printfn ""

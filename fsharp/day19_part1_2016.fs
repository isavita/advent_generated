
module Day

let readInput (filename: string) : int =
    use file = System.IO.File.OpenText filename
    let totalElves = file.ReadLine() |> int
    totalElves

let findWinningElf (totalElves: int) : int =
    let mutable highestPowerOfTwo = 1
    while highestPowerOfTwo * 2 <= totalElves do
        highestPowerOfTwo <- highestPowerOfTwo * 2
    (totalElves - highestPowerOfTwo) * 2 + 1

let totalElves = readInput "input.txt"
let winner = findWinningElf totalElves
printfn "%d" winner


open System
open System.Text.RegularExpressions
open System.IO

let cubeRegex = new Regex(@"(\d+) (red|green|blue)")
let gameRegex = new Regex(@"Game (\d+): (.+)")

let isValidRound (round: string) =
    let mutable red, green, blue = 0, 0, 0
    for cube in cubeRegex.Matches(round) do
        let count = int cube.Groups.[1].Value
        match cube.Groups.[2].Value with
        | "red" -> red <- red + count
        | "green" -> green <- green + count
        | "blue" -> blue <- blue + count
        | _ -> ()
    red <= 12 && green <= 13 && blue <= 14

let solve (lines: string[]) =
    let mutable totalSum = 0
    for line in lines do
        let matchGame = gameRegex.Match(line)
        if matchGame.Success then
            let gameId = int matchGame.Groups.[1].Value
            let rounds = matchGame.Groups.[2].Value.Split(';')
            if Array.forall isValidRound rounds then
                totalSum <- totalSum + gameId
    totalSum

let main() =
    let lines = File.ReadAllLines("input.txt")
    printfn "%d" (solve lines)

main()

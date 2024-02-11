module Day2

let input = System.IO.File.ReadAllLines "input.txt"

let isValidPart1 (line:string) =
    let parts = line.Split(' ')
    let range = parts.[0].Split('-') |> Array.map int
    let charToCheck = parts.[1].[0]
    let password = parts.[2]
    let count = password.ToCharArray() |> Array.filter (fun c -> c = charToCheck) |> Array.length
    count >= range.[0] && count <= range.[1]

let part1Result = input |> Array.filter isValidPart1 |> Array.length

let isValidPart2 (line:string) =
    let parts = line.Split(' ')
    let positions = parts.[0].Split('-') |> Array.map (fun x -> int x - 1)
    let charToCheck = parts.[1].[0]
    let password = parts.[2]
    (password.[positions.[0]] = charToCheck) <> (password.[positions.[1]] = charToCheck)

let part2Result = input |> Array.filter isValidPart2 |> Array.length

printfn "%d" part1Result
printfn "%d" part2Result
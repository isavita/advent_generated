
module Day2

let input = System.IO.File.ReadAllLines "input.txt"

let isValid (password:string) =
    let parts = password.Split(' ')
    let range = parts.[0].Split('-') |> Array.map int
    let min = range.[0]
    let max = range.[1]
    let charToCheck = parts.[1].[0]
    let passwordToCheck = parts.[2]
    let count = passwordToCheck |> Seq.filter ((=) charToCheck) |> Seq.length
    count >= min && count <= max

let result = input |> Array.filter isValid |> Array.length
printfn "%d" result

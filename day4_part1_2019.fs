
module Day4

let input = System.IO.File.ReadAllText("input.txt")
let range = input.Split('-') |> Array.map int

let isValidPassword (password: int) =
    let passwordStr = password.ToString()
    let mutable hasAdjacent = false
    let mutable neverDecrease = true
    for i in 1..5 do
        if passwordStr.[i] = passwordStr.[i-1] then
            hasAdjacent <- true
        if int(passwordStr.[i]) < int(passwordStr.[i-1]) then
            neverDecrease <- false
    hasAdjacent && neverDecrease

let mutable count = 0
for password in range.[0]..range.[1] do
    if isValidPassword password then
        count <- count + 1

printfn "%d" count

module Day4

let input = System.IO.File.ReadAllText("input.txt")

let isValidPasswordPart1 (password: string) =
    let mutable hasAdjacent = false
    let mutable neverDecrease = true
    for i in 1..5 do
        if password.[i] = password.[i-1] then
            hasAdjacent <- true
        if int(password.[i]) < int(password.[i-1]) then
            neverDecrease <- false
    hasAdjacent && neverDecrease

let isValidPasswordPart2 (password: string) =
    let mutable hasAdjacent = false
    let mutable neverDecrease = true
    let mutable groupCount = 1
    for i in 1..5 do
        if password.[i] = password.[i-1] then
            groupCount <- groupCount + 1
        else
            if groupCount = 2 then
                hasAdjacent <- true
            groupCount <- 1
        if int(password.[i]) < int(password.[i-1]) then
            neverDecrease <- false
    (hasAdjacent || groupCount = 2) && neverDecrease

let range = input.Split('-') |> Array.map int
let mutable countPart1 = 0
let mutable countPart2 = 0

for password in range[0]..range[1] do
    let passwordStr = password.ToString()
    if isValidPasswordPart1 passwordStr then
        countPart1 <- countPart1 + 1
    if isValidPasswordPart2 passwordStr then
        countPart2 <- countPart2 + 1

printfn "%d" countPart1
printfn "%d" countPart2
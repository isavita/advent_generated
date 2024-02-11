
module Day20

let input = System.IO.File.ReadAllText "input.txt"
let target = int input

let rec sumFactors n =
    let rec aux i acc =
        if i * i > n then acc
        else if n % i = 0 then
            let factor1 = i
            let factor2 = n / i
            if factor1 = factor2 then aux (i + 1) (acc + factor1)
            else aux (i + 1) (acc + factor1 + factor2)
        else aux (i + 1) acc
    aux 1 0

let rec findHouse n =
    let totalPresents = sumFactors n * 10
    if totalPresents >= target then n
    else findHouse (n + 1)

let result = findHouse 1
printfn "%d" result

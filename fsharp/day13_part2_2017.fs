
module Day13

let input = System.IO.File.ReadAllLines "input.txt"

let parseInput (input: string[]) =
    input
    |> Array.map (fun line -> 
        let parts = line.Split(": ")
        (int parts.[0], int parts.[1])
    )

let severity (firewall: (int * int)[]) =
    firewall
    |> Array.filter (fun (depth, range) -> depth % ((range - 1) * 2) = 0)
    |> Array.sumBy (fun (depth, range) -> depth * range)

let rec delayPacket (firewall: (int * int)[]) (delay: int) =
    firewall
    |> Array.forall (fun (depth, range) -> (depth + delay) % ((range - 1) * 2) <> 0)

let rec findDelay (firewall: (int * int)[]) (delay: int) =
    if delayPacket firewall delay then delay
    else findDelay firewall (delay + 1)

let parsedInput = parseInput input
let resultPart1 = severity parsedInput
let resultPart2 = findDelay parsedInput 0

printfn "%d" resultPart1
printfn "%d" resultPart2

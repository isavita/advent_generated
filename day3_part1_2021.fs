
module Day3

let input = System.IO.File.ReadAllLines "input.txt"

let getGammaRate () =
    let length = input.[0].Length
    let mutable gamma = ""
    for i in 0..(length-1) do
        let count0 = input |> Array.map (fun x -> x.[i]) |> Array.filter (fun x -> x = '0') |> Array.length
        let count1 = input |> Array.map (fun x -> x.[i]) |> Array.filter (fun x -> x = '1') |> Array.length
        if count0 >= count1 then gamma <- gamma + "0"
        else gamma <- gamma + "1"
    gamma

let getEpsilonRate () =
    let length = input.[0].Length
    let mutable epsilon = ""
    for i in 0..(length-1) do
        let count0 = input |> Array.map (fun x -> x.[i]) |> Array.filter (fun x -> x = '0') |> Array.length
        let count1 = input |> Array.map (fun x -> x.[i]) |> Array.filter (fun x -> x = '1') |> Array.length
        if count0 <= count1 then epsilon <- epsilon + "0"
        else epsilon <- epsilon + "1"
    epsilon

let gammaRate = getGammaRate ()
let epsilonRate = getEpsilonRate ()
let powerConsumption = System.Convert.ToInt32(gammaRate, 2) * System.Convert.ToInt32(epsilonRate, 2)
printfn "%d" powerConsumption

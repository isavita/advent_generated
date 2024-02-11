module Day1

let calculateFuelForMass mass = max 0 (mass / 3 - 2)

let rec calculateTotalFuelForMass mass =
    let fuel = calculateFuelForMass mass
    if fuel <= 0 then 0
    else fuel + calculateTotalFuelForMass fuel

let input = System.IO.File.ReadAllLines "input.txt"
let totalFuel = input |> Array.map int |> Array.sumBy calculateTotalFuelForMass
printfn "%d" totalFuel
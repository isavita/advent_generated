
open System
open System.IO

type Ingredient = {
    capacity   : int
    durability : int
    flavor     : int
    texture    : int
    calories   : int
}

let parseLine (line:string) =
    let parts = line.Split([|' '; ':'; ','|], StringSplitOptions.RemoveEmptyEntries)
    {
        capacity   = int parts.[2]
        durability = int parts.[4]
        flavor     = int parts.[6]
        texture    = int parts.[8]
        calories   = int parts.[10]
    }

let ingredients =
    File.ReadAllLines("input.txt")
    |> Array.map parseLine

let totalTeaspoons = 100
let targetCalories = 500
let n = ingredients.Length
let teaspoons = Array.zeroCreate<int> n

let score (caps:int) (durs:int) (flas:int) (texs:int) =
    let caps = if caps < 0 then 0 else caps
    let durs = if durs < 0 then 0 else durs
    let flas = if flas < 0 then 0 else flas
    let texs = if texs < 0 then 0 else texs
    caps * durs * flas * texs

let rec dfs idx remaining =
    if idx = n - 1 then
        teaspoons.[idx] <- remaining
        let cal =
            Array.fold2 (fun acc ing tsp -> acc + ing.calories * tsp) 0 ingredients teaspoons
        if cal = targetCalories then
            let cap = Array.fold2 (fun acc ing tsp -> acc + ing.capacity * tsp) 0 ingredients teaspoons
            let dur = Array.fold2 (fun acc ing tsp -> acc + ing.durability * tsp) 0 ingredients teaspoons
            let fla = Array.fold2 (fun acc ing tsp -> acc + ing.flavor * tsp) 0 ingredients teaspoons
            let tex = Array.fold2 (fun acc ing tsp -> acc + ing.texture * tsp) 0 ingredients teaspoons
            score cap dur fla tex
        else 0
    else
        let mutable best = 0
        for i in 0 .. remaining do
            teaspoons.[idx] <- i
            let cur = dfs (idx + 1) (remaining - i)
            if cur > best then best <- cur
        best

[<EntryPoint>]
let main _ =
    let result = dfs 0 totalTeaspoons
    printfn "%d" result
    0

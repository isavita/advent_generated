
open System
open System.IO

type Ingredient = {
    Capacity   : int
    Durability : int
    Flavor     : int
    Texture    : int
}

let readIngredients (file:string) =
    File.ReadAllLines(file)
    |> Array.map (fun line ->
        let parts = line.Split([|':' ; ',' ; ' '; '\t'|], StringSplitOptions.RemoveEmptyEntries)
        {
            Capacity   = int parts.[2]
            Durability = int parts.[4]
            Flavor     = int parts.[6]
            Texture    = int parts.[8]
        })
    |> Array.toList

let score (ings:Ingredient list) (ts:int[]) =
    let mutable cap = 0
    let mutable dur = 0
    let mutable fla = 0
    let mutable tex = 0
    for i in 0 .. ings.Length-1 do
        let ing = ings.[i]
        let t = ts.[i]
        cap <- cap + ing.Capacity   * t
        dur <- dur + ing.Durability * t
        fla <- fla + ing.Flavor     * t
        tex <- tex + ing.Texture    * t
    let max0 x = if x < 0 then 0 else x
    int64 (max0 cap) * int64 (max0 dur) * int64 (max0 fla) * int64 (max0 tex)

let maxScore (ings:Ingredient list) total =
    let n = ings.Length
    let ts = Array.create n 0
    let rec dfs idx remaining =
        if idx = n-1 then
            ts.[idx] <- remaining
            score ings ts
        else
            let mutable best = 0L
            for i in 0 .. remaining do
                ts.[idx] <- i
                let cur = dfs (idx+1) (remaining-i)
                if cur > best then best <- cur
            best
    dfs 0 total

[<EntryPoint>]
let main argv =
    let ingredients = readIngredients "input.txt"
    let result = maxScore ingredients 100
    printfn "%d" result
    0

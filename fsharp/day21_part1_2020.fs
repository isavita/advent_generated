
open System
open System.Collections.Generic
open System.IO
open System.Linq

let readLines (filePath: string) = File.ReadAllLines(filePath)

let solve (lines: string[]) =
    let allergenMap = new Dictionary<string, HashSet<string>>()
    let ingredientCount = new Dictionary<string, int>()
    let mutable safeIngredients = new HashSet<string>()

    for line in lines do
        let parts = line.Split(" (contains ")
        let ingredients = parts.[0].Split(' ') |> Array.toList
        let allergens = if parts.Length > 1 then parts.[1].Substring(0, parts.[1].Length - 1).Split(", ") else [||]

        for ingredient in ingredients do
            if not (ingredientCount.ContainsKey(ingredient)) then
                ingredientCount.[ingredient] <- 1
                safeIngredients.Add(ingredient) |> ignore
            else
                ingredientCount.[ingredient] <- ingredientCount.[ingredient] + 1

        for allergen in allergens do
            if not (allergenMap.ContainsKey(allergen)) then
                allergenMap.[allergen] <- new HashSet<string>(ingredients)
            else
                let ingredientsSet = new HashSet<string>(ingredients)
                allergenMap.[allergen].IntersectWith(ingredientsSet)

    for ingredients in allergenMap.Values do
        for ingredient in ingredients do
            safeIngredients.Remove(ingredient) |> ignore

    safeIngredients
    |> Seq.sumBy (fun ingredient -> ingredientCount.[ingredient])

let main() =
    let lines = readLines "input.txt"
    printfn "%d" (solve lines)

main()

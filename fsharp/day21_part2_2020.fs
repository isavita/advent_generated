open System
open System.IO

type FoodItem = { Ingredients: string list; Allergens: string list }

let parseLine (line: string) =
  let parts = line.Split([| "(contains " |], StringSplitOptions.None)
  let ingredients = parts.[0].Trim().Split([|' ' |], StringSplitOptions.RemoveEmptyEntries) |> Array.toList
  let allergensPart = parts.[1].TrimEnd(')')
  let allergens = allergensPart.Split([| ", " |], StringSplitOptions.None) |> Array.toList
  { Ingredients = ingredients; Allergens = allergens }

let rec resolveAllergens (cands: Map<string, Set<string>>) : Map<string, string> =
  if Map.isEmpty cands then Map.empty
  else
    let singles = cands |> Map.toList |> List.filter (fun (_, s) -> Set.count s = 1)
    if singles.IsEmpty then failwith "Could not resolve allergens"
    let allergen, s = singles.Head
    let ingredient = Set.minElement s
    let updated = cands |> Map.remove allergen |> Map.map (fun _ set -> Set.remove ingredient set)
    let rest = resolveAllergens updated
    rest |> Map.add allergen ingredient

[<EntryPoint>]
let main argv =
  let lines = File.ReadAllLines("input.txt") |> Array.toList
  let foods = lines |> List.map parseLine

  let ingredientCounts =
    foods
    |> List.collect (fun f -> f.Ingredients)
    |> List.countBy id
    |> Map.ofList

  let allergenCandidates =
    foods
    |> List.fold (fun acc item ->
      item.Allergens |> List.fold (fun acc2 allergen ->
        match Map.tryFind allergen acc2 with
        | None -> acc2 |> Map.add allergen (Set.ofList item.Ingredients)
        | Some s -> acc2 |> Map.add allergen (Set.intersect s (Set.ofList item.Ingredients))
      ) acc
    ) Map.empty<string, Set<string>>

  let unsafeIngredients =
    allergenCandidates |> Map.fold (fun acc _ s -> Set.union acc s) Set.empty

  let safeCount =
    ingredientCounts
    |> Map.filter (fun ing _ -> not (Set.contains ing unsafeIngredients))
    |> Map.toList
    |> List.sumBy snd

  printfn "%d" safeCount

  let resolvedAllergens = resolveAllergens allergenCandidates
  let canonicalList =
    resolvedAllergens |> Map.toList |> List.sortBy fst |> List.map snd |> String.concat ","
  printfn "%s" canonicalList

  0
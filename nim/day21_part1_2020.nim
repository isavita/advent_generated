
import std/[strutils, tables, sets, os]

proc contains[T](s: openArray[T], x: T): bool =
  for item in s:
    if item == x:
      return true
  false

proc main() =
  let file = open("input.txt")
  defer: file.close()

  var 
    allergenMap = initTable[string, HashSet[string]]()
    ingredientCount = initCountTable[string]()
    safeIngredients = initHashSet[string]()

  for line in file.lines:
    let parts = line.split(" (contains ")
    let ingredients = parts[0].splitWhitespace()
    var allergens: seq[string] = @[]

    if parts.len > 1:
      allergens = parts[1][0..^2].split(", ")

    for ingredient in ingredients:
      ingredientCount.inc(ingredient)
      safeIngredients.incl(ingredient)

    for allergen in allergens:
      if allergen notin allergenMap:
        var ingredientSet = initHashSet[string]()
        for ingredient in ingredients:
          ingredientSet.incl(ingredient)
        allergenMap[allergen] = ingredientSet
      else:
        var toRemove = initHashSet[string]()
        for ingredient in allergenMap[allergen]:
          if not contains(ingredients, ingredient):
            toRemove.incl(ingredient)
        allergenMap[allergen].excl(toRemove)

  for ingredients in allergenMap.values:
    for ingredient in ingredients:
      safeIngredients.excl(ingredient)

  var count = 0
  for ingredient in safeIngredients:
    count += ingredientCount[ingredient]

  echo count

main()

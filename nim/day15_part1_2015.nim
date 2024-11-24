
import strutils, sequtils, algorithm

type Ingredient = object
  name: string
  capacity: int
  durability: int
  flavor: int
  texture: int

proc readIngredients(filename: string): seq[Ingredient] =
  for line in lines filename:
    let parts = line.splitWhitespace
    if parts.len < 11: continue
    result.add(Ingredient(
      name: parts[0],
      capacity: parseInt(parts[2][0..<parts[2].len - 1]),
      durability: parseInt(parts[4][0..<parts[4].len - 1]),
      flavor: parseInt(parts[6][0..<parts[6].len - 1]),
      texture: parseInt(parts[8][0..<parts[8].len - 1])
    ))

proc score(ingredients: seq[Ingredient], teaspoons: seq[int]): int =
  var capacity, durability, flavor, texture: int
  for i in 0..<ingredients.len:
    capacity += ingredients[i].capacity * teaspoons[i]
    durability += ingredients[i].durability * teaspoons[i]
    flavor += ingredients[i].flavor * teaspoons[i]
    texture += ingredients[i].texture * teaspoons[i]
  max(0, capacity) * max(0, durability) * max(0, flavor) * max(0, texture)

proc calculateMaxScore(ingredients: seq[Ingredient], index: int, remaining: int, teaspoons: seq[int]): int =
  if index == ingredients.len - 1:
    return score(ingredients, teaspoons & remaining)
  var maxScore = 0
  for i in 0..remaining:
    maxScore = max(maxScore, calculateMaxScore(ingredients, index + 1, remaining - i, teaspoons & i))
  maxScore

proc findMaxScore(ingredients: seq[Ingredient], totalTeaspoons: int): int =
  calculateMaxScore(ingredients, 0, totalTeaspoons, @[])


proc main() =
  let ingredients = readIngredients("input.txt")
  echo findMaxScore(ingredients, 100)

main()

import strutils, sequtils, math

type Ingredient = object
  capacity, durability, flavor, texture, calories: int

proc parseIngredient(line: string): Ingredient =
  let parts = line.split(": ")
  let properties = parts[1].split(", ")
  let capacity = parseInt(properties[0].split(" ")[1])
  let durability = parseInt(properties[1].split(" ")[1])
  let flavor = parseInt(properties[2].split(" ")[1])
  let texture = parseInt(properties[3].split(" ")[1])
  let calories = parseInt(properties[4].split(" ")[1])
  Ingredient(capacity: capacity, durability: durability, flavor: flavor, texture: texture, calories: calories)

proc calculateScore(ingredients: seq[Ingredient], amounts: seq[int]): int =
  var capacity, durability, flavor, texture, calories: int
  for i, ingredient in ingredients:
    capacity += ingredient.capacity * amounts[i]
    durability += ingredient.durability * amounts[i]
    flavor += ingredient.flavor * amounts[i]
    texture += ingredient.texture * amounts[i]
    calories += ingredient.calories * amounts[i]

  if calories != 500:
    return 0

  if capacity < 0: capacity = 0
  if durability < 0: durability = 0
  if flavor < 0: flavor = 0
  if texture < 0: texture = 0

  capacity * durability * flavor * texture

proc findBestScore(ingredients: seq[Ingredient], amounts: var seq[int], index, teaspoons: int, bestScore: var int) =
  if index == ingredients.len - 1:
    amounts[index] = teaspoons
    let score = calculateScore(ingredients, amounts)
    if score > bestScore:
      bestScore = score
    return

  for i in 0..teaspoons:
    amounts[index] = i
    findBestScore(ingredients, amounts, index + 1, teaspoons - i, bestScore)

when isMainModule:
  let file = readFile("input.txt")
  let lines = file.splitLines()
  var ingredients: seq[Ingredient] = @[]
  for line in lines:
    ingredients.add(parseIngredient(line))

  var bestScore = 0
  var amounts: seq[int] = newSeq[int](ingredients.len)
  findBestScore(ingredients, amounts, 0, 100, bestScore)

  echo bestScore
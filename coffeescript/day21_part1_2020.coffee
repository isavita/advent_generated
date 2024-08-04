fs = require 'fs'

fileContent = fs.readFileSync('input.txt', 'utf8').split '\n'

allergenMap = {}
ingredientCount = {}
safeIngredients = {}

for line in fileContent
  [ingredientsPart, allergensPart] = line.split ' (contains '
  ingredients = ingredientsPart.split ' '
  allergens = if allergensPart then allergensPart.slice(0, -1).split ', ' else []

  for ingredient in ingredients
    ingredientCount[ingredient] = (ingredientCount[ingredient] or 0) + 1
    safeIngredients[ingredient] = true

  for allergen in allergens
    if not allergenMap[allergen]
      allergenMap[allergen] = {}
      for ingredient in ingredients
        allergenMap[allergen][ingredient] = true
    else
      for ingredient of allergenMap[allergen]
        if not (ingredient in ingredients)
          delete allergenMap[allergen][ingredient]

for allergen, ingredients of allergenMap
  for ingredient of ingredients
    delete safeIngredients[ingredient]

count = 0
for ingredient of safeIngredients
  count += ingredientCount[ingredient]

console.log count
fs = require 'fs'

removeIngredientFromAll = (allergenMap, ingredient) ->
  for ingredients of allergenMap
    delete allergenMap[ingredients][ingredient]

contains = (slice, str) ->
  str in slice

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err

  allergenMap = {}
  ingredientAllergen = {}
  lines = data.trim().split '\n'

  for line in lines
    parts = line.split ' (contains '
    ingredients = parts[0].split ' '
    allergens = if parts.length > 1 then parts[1].slice(0, -1).split ', ' else []

    for allergen in allergens
      if not allergenMap[allergen]
        allergenMap[allergen] = {}
        for ingredient in ingredients
          allergenMap[allergen][ingredient] = true
      else
        for ingredient of allergenMap[allergen]
          delete allergenMap[allergen][ingredient] unless contains ingredients, ingredient

  while Object.keys(allergenMap).length > 0
    for allergen, ingredients of allergenMap
      if Object.keys(ingredients).length == 1
        for ingredient of ingredients
          ingredientAllergen[allergen] = ingredient
          removeIngredientFromAll allergenMap, ingredient
        delete allergenMap[allergen]

  allergens = Object.keys ingredientAllergen
  allergens.sort()

  result = (ingredientAllergen[allergen] for allergen in allergens)
  console.log result.join(',')
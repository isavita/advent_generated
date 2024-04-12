fs = require 'fs'

class Ingredient
  constructor: (@name, @capacity, @durability, @flavor, @texture) ->

readIngredients = (filename) ->
  data = fs.readFileSync filename, 'utf8'
  ingredients = []
  for line in data.trim().split '\n'
    parts = line.split /\s+/
    continue if parts.length < 11
    capacity = parseInt parts[2].slice(0, -1)
    durability = parseInt parts[4].slice(0, -1)
    flavor = parseInt parts[6].slice(0, -1)
    texture = parseInt parts[8].slice(0, -1)
    ingredient = new Ingredient parts[0], capacity, durability, flavor, texture
    ingredients.push ingredient
  ingredients

score = (ingredients, teaspoons) ->
  capacity = durability = flavor = texture = 0
  for i, ingredient of ingredients
    capacity += ingredient.capacity * teaspoons[i]
    durability += ingredient.durability * teaspoons[i]
    flavor += ingredient.flavor * teaspoons[i]
    texture += ingredient.texture * teaspoons[i]
  capacity = 0 if capacity < 0
  durability = 0 if durability < 0
  flavor = 0 if flavor < 0
  texture = 0 if texture < 0
  capacity * durability * flavor * texture

calculateMaxScore = (ingredients, index, remaining, teaspoons) ->
  if index == ingredients.length - 1
    teaspoons.push remaining
    return score ingredients, teaspoons
  maxScore = 0
  for i in [0..remaining]
    newTeaspoons = teaspoons.concat i
    currentScore = calculateMaxScore ingredients, index + 1, remaining - i, newTeaspoons
    maxScore = Math.max maxScore, currentScore
  maxScore

findMaxScore = (ingredients, totalTeaspoons) ->
  calculateMaxScore ingredients, 0, totalTeaspoons, []

main = ->
  ingredients = readIngredients 'input.txt'
  maxScore = findMaxScore ingredients, 100
  console.log maxScore

main()
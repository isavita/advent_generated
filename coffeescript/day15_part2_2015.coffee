fs = require 'fs'

class Ingredient
    constructor: (@name, @capacity, @durability, @flavor, @texture, @calories) ->

readIngredients = (filename) ->
    data = fs.readFileSync(filename, 'utf8')
    lines = data.split('\n')
    ingredients = []

    for line in lines
        parts = line.split(' ')
        if parts.length < 11
            continue

        capacity = parseInt(parts[2].slice(0, -1))
        durability = parseInt(parts[4].slice(0, -1))
        flavor = parseInt(parts[6].slice(0, -1))
        texture = parseInt(parts[8].slice(0, -1))
        calories = parseInt(parts[10])

        ingredients.push new Ingredient(parts[0], capacity, durability, flavor, texture, calories)

    ingredients

calculateMaxScore = (ingredients, index, remaining, teaspoons, targetCalories) ->
    if index == ingredients.length - 1
        teaspoons.push remaining
        if calculateCalories(ingredients, teaspoons) == targetCalories
            return score(ingredients, teaspoons)
        return 0

    maxScore = 0
    for i in [0..remaining]
        currentScore = calculateMaxScore(ingredients, index + 1, remaining - i, teaspoons.concat(i), targetCalories)
        if currentScore > maxScore
            maxScore = currentScore
    maxScore

score = (ingredients, teaspoons) ->
    capacity = durability = flavor = texture = 0
    for ingredient, i in ingredients
        capacity += ingredient.capacity * teaspoons[i]
        durability += ingredient.durability * teaspoons[i]
        flavor += ingredient.flavor * teaspoons[i]
        texture += ingredient.texture * teaspoons[i]

    capacity = Math.max(capacity, 0)
    durability = Math.max(durability, 0)
    flavor = Math.max(flavor, 0)
    texture = Math.max(texture, 0)

    capacity * durability * flavor * texture

calculateCalories = (ingredients, teaspoons) ->
    calories = 0
    for ingredient, i in ingredients
        calories += ingredient.calories * teaspoons[i]
    calories

ingredients = readIngredients('input.txt')
maxScore = calculateMaxScore(ingredients, 0, 100, [], 500)
console.log maxScore
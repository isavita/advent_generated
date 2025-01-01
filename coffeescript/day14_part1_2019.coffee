
fs = require 'fs'

parseChemical = (s) ->
  [amount, name] = s.split ' '
  amount: parseInt(amount), name: name

main = ->
  try
    data = fs.readFileSync('input.txt', 'utf8').trim().split '\n'
  catch error
    console.log "Error opening file:", error
    return

  reactions = {}
  ingredients = {}

  for line in data
    [inputStr, outputStr] = line.split ' => '
    output = parseChemical outputStr
    inputs = (parseChemical inStr for inStr in inputStr.split ', ')
    reactions[output.name] = output
    ingredients[output.name] = inputs

  console.log calculateOre "FUEL", 1, reactions, ingredients, {}

calculateOre = (chem, amount, reactions, ingredients, surplus) ->
  if chem is "ORE"
    return amount

  surplus[chem] ?= 0
  if surplus[chem] >= amount
    surplus[chem] -= amount
    return 0

  amount -= surplus[chem]
  surplus[chem] = 0
  reaction = reactions[chem]
  times = Math.ceil(amount / reaction.amount)
  ore = 0

  for ingredient in ingredients[chem]
    ore += calculateOre ingredient.name, ingredient.amount * times, reactions, ingredients, surplus

  surplus[chem] += times * reaction.amount - amount
  ore
main()

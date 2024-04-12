fs = require 'fs'

solve = (input) ->
  [reverseGraph, startingMols] = parseInput input

  productToReactant = {}
  for react, products of reverseGraph
    for p in products
      throw "dup found" if productToReactant[p]
      productToReactant[p] = react

  allProducts = Object.keys productToReactant
  start = startingMols.join ""
  mol = start

  steps = 0
  while mol isnt "e"
    changeMade = false
    for prod in allProducts
      matches = mol.match new RegExp(prod, 'g')
      count = if matches then matches.length else 0
      if count > 0
        changeMade = true
        steps += count
        mol = mol.replace new RegExp(prod, 'g'), productToReactant[prod]
        break

    unless changeMade
      allProducts = shuffleSlice allProducts
      mol = start
      steps = 0

  steps

parseInput = (input) ->
  blocks = input.trim().split "\n\n"
  startingMaterial = splitMolecules blocks[1]

  graph = {}
  for l in blocks[0].split "\n"
    [key, value] = l.split " => "
    graph[key] = (graph[key] or []).concat value

  [graph, startingMaterial]

splitMolecules = (input) ->
  molecules = []
  for char, i in input
    if char.match /[A-Z]/
      molecules.push char
    else
      molecules[molecules.length - 1] += char
  molecules

shuffleSlice = (array) ->
  array.sort -> Math.random() - 0.5

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  console.log solve data.trim()
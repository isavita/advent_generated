
fs = require 'fs'

# Parse the input file.
parseInput = (filename) ->
  lines = fs.readFileSync(filename, 'utf-8').trim().split('\n')
  programs = {}
  for line in lines
    match = line.match /^(\w+) \((\d+)\)(?: -> (.*))?$/
    name = match[1]
    weight = parseInt(match[2])
    children = match[3]?.split(', ') || []
    programs[name] = { weight, children, name }  # Store name for easier access
  programs

# Find the root program (the one not listed as a child of any other program).
findRoot = (programs) ->
  allChildren = new Set()
  for program of programs
    for child in programs[program].children
      allChildren.add child
  for program of programs
    if not allChildren.has program
      return program

# Calculate the total weight of a program, including its children.
calculateTotalWeight = (programs, programName) ->
  program = programs[programName]
  totalWeight = program.weight
  for child in program.children
    totalWeight += calculateTotalWeight(programs, child)
  program.totalWeight = totalWeight # Cache to avoid recomputation
  totalWeight

# Find the imbalanced program and the correct weight.
findImbalance = (programs, rootName) ->
  root = programs[rootName]
  # Helper function to recursively check balance
  checkBalance = (programName) ->
    program = programs[programName]
    childWeights = []
    childNames = []

    for childName in program.children
      childWeights.push calculateTotalWeight(programs, childName)
      childNames.push childName
    
    if childWeights.length > 0
      # Check for imbalance among children
      firstWeight = childWeights[0]
      imbalancedChild = null
      correctWeight = null

      for i in [1...childWeights.length]
        if childWeights[i] != firstWeight
          imbalancedChild = if childWeights.filter((w) -> w is firstWeight).length > childWeights.filter((w) -> w is childWeights[i]).length then childNames[i] else childNames[0]
          correctWeight = if childWeights.filter((w) -> w is firstWeight).length > childWeights.filter((w) -> w is childWeights[i]).length then firstWeight else childWeights[i]
          break
      # if found an imbalance among the children then recurse
      if imbalancedChild
          result = checkBalance(imbalancedChild)
          #if a result, that is it!
          return result if result
          # If the recursion didn't find further imbalance, the current child is the culprit.
          diff = correctWeight - programs[imbalancedChild].totalWeight
          return programs[imbalancedChild].weight + diff

  checkBalance(rootName)
  
# --- Main Execution ---
programs = parseInput('input.txt')
rootName = findRoot(programs)
console.log "Part 1: Root program name: #{rootName}"

correctWeight = findImbalance(programs, rootName)
console.log "Part 2: Corrected weight: #{correctWeight}"


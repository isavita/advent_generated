
import strutils
import tables
import math

type
  Chemical = object
    name: string
    amount: int

proc parseChemical(s: string): Chemical =
  let parts = s.strip().split(' ')
  result = Chemical(name: parts[1], amount: parseInt(parts[0]))

proc calculateOre(chem: string, amount: int, reactions: Table[string, Chemical], ingredients: Table[string, seq[Chemical]], surplus: var Table[string, int]): int =
  if chem == "ORE":
    return amount

  var neededAmount = amount
  if surplus.contains(chem):
    let available = surplus[chem]
    if available >= neededAmount:
      surplus[chem] = available - neededAmount
      return 0
    else:
      neededAmount -= available
      surplus[chem] = 0
  
  let reaction = reactions[chem]
  let reactionAmount = reaction.amount
  # Ceiling division: (neededAmount + reactionAmount - 1) div reactionAmount
  let times = (neededAmount + reactionAmount - 1) div reactionAmount
  
  var oreCost = 0
  for ingredient in ingredients[chem]:
    oreCost += calculateOre(ingredient.name, ingredient.amount * times, reactions, ingredients, surplus)

  let producedAmount = times * reactionAmount
  surplus[chem] = surplus.getOrDefault(chem) + producedAmount - neededAmount
  
  return oreCost

proc maxFuel(reactions: Table[string, Chemical], ingredients: Table[string, seq[Chemical]], oreAvailable: int): int =
  var low = 0
  # Initial high estimate: assumes 1 FUEL costs at least 1 ORE, likely much more.
  # A more refined estimate could be oreAvailable div calculateOre("FUEL", 1, ... with empty surplus),
  # but a large enough number works too. oreAvailable itself is a safe upper bound.
  var high = oreAvailable 
  var resultFuel = 0

  # Estimate cost of 1 FUEL to refine the upper bound
  var initialSurplus: Table[string, int] = initTable[string, int]()
  let costPerFuel = calculateOre("FUEL", 1, reactions, ingredients, initialSurplus)
  if costPerFuel > 0:
      high = min(high, oreAvailable div costPerFuel * 2) # Refined upper bound, *2 for safety margin

  while low <= high:
    let mid = low + (high - low) div 2
    if mid == 0: # Avoid calculating for 0 fuel if low starts at 0
        low = 1
        continue
        
    var currentSurplus = initTable[string, int]()
    let oreNeeded = calculateOre("FUEL", mid, reactions, ingredients, currentSurplus)

    if oreNeeded <= oreAvailable:
      resultFuel = mid # Found a possible answer
      low = mid + 1    # Try for more fuel
    else:
      high = mid - 1    # Need less fuel

  return resultFuel


proc main() =
  var reactions = initTable[string, Chemical]()
  var ingredients = initTable[string, seq[Chemical]]()

  for line in lines "input.txt":
    let parts = line.strip().split(" => ")
    let output = parseChemical(parts[1])
    reactions[output.name] = output

    var currentIngredients: seq[Chemical] = @[]
    for inputStr in parts[0].split(", "):
      currentIngredients.add(parseChemical(inputStr))
    ingredients[output.name] = currentIngredients

  let oreAvailable = 1_000_000_000_000 # Use _ for readability
  let result = maxFuel(reactions, ingredients, oreAvailable)
  echo result

when isMainModule:
  main()


import std/[strutils, tables, math, os]

type
  Chemical = object
    name: string
    amount: int

proc parseChemical(s: string): Chemical =
  let parts = s.split(" ")
  result = Chemical(name: parts[1], amount: parts[0].parseInt())

proc calculateOre(chem: string, amount: int, reactions: Table[string, Chemical], 
                  ingredients: Table[string, seq[Chemical]], surplus: var Table[string, int]): int =
  if chem == "ORE":
    return amount

  if surplus.getOrDefault(chem) >= amount:
    surplus[chem] -= amount
    return 0

  var remainingAmount = amount - surplus.getOrDefault(chem)
  surplus[chem] = 0
  let reaction = reactions[chem]
  let times = ceil(remainingAmount / reaction.amount).int

  for ingredient in ingredients[chem]:
    result += calculateOre(ingredient.name, ingredient.amount * times, 
                            reactions, ingredients, surplus)

  surplus[chem] += times * reaction.amount - remainingAmount
  return result

proc main() =
  var 
    reactions = initTable[string, Chemical]()
    ingredients = initTable[string, seq[Chemical]]()
    surplus = initTable[string, int]()

  let file = open("input.txt")
  defer: file.close()

  for line in file.lines:
    let parts = line.split(" => ")
    let output = parseChemical(parts[1])
    var inputChemicals: seq[Chemical]

    for input in parts[0].split(", "):
      inputChemicals.add(parseChemical(input))

    reactions[output.name] = output
    ingredients[output.name] = inputChemicals

  echo calculateOre("FUEL", 1, reactions, ingredients, surplus)

main()

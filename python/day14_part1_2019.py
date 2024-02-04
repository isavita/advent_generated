
import sys

def parseChemical(s):
    parts = s.split()
    amount = int(parts[0])
    return {"name": parts[1], "amount": amount}

def calculateOre(chem, amount, reactions, ingredients, surplus):
    if chem == "ORE":
        return amount
    
    if surplus.get(chem, 0) >= amount:
        surplus[chem] -= amount
        return 0
    
    amount -= surplus.get(chem, 0)
    surplus[chem] = 0
    reaction = reactions[chem]
    times = (amount + reaction["amount"] - 1) // reaction["amount"]
    ore = 0

    for ingredient in ingredients[chem]:
        ore += calculateOre(ingredient["name"], ingredient["amount"] * times, reactions, ingredients, surplus)
    
    surplus[chem] += times * reaction["amount"] - amount
    return ore

with open("input.txt", "r") as file:
    reactions = {}
    ingredients = {}
    surplus = {}
    
    for line in file:
        parts = line.strip().split(" => ")
        output = parseChemical(parts[1])
        inputs = [parseChemical(x) for x in parts[0].split(", ")]
        reactions[output["name"]] = output
        ingredients[output["name"]] = inputs

    print(calculateOre("FUEL", 1, reactions, ingredients, surplus))

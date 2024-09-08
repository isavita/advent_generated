from itertools import product
from functools import reduce
import operator

def parse_input(filename):
    ingredients = {}
    with open(filename, 'r') as f:
        for line in f:
            name, props = line.strip().split(': ')
            ingredients[name] = [int(prop.split()[1]) for prop in props.split(', ')]
    return ingredients

def calculate_score(combo, ingredients):
    totals = [max(0, sum(a*b[i] for a, b in zip(combo, ingredients.values())))
              for i in range(4)]
    return reduce(operator.mul, totals)

def solve(ingredients, calorie_limit=None):
    best_score = 0
    for combo in product(range(101), repeat=len(ingredients)-1):
        if sum(combo) > 100:
            continue
        combo = combo + (100 - sum(combo),)
        
        if calorie_limit is not None:
            calories = sum(a*b[4] for a, b in zip(combo, ingredients.values()))
            if calories != calorie_limit:
                continue
        
        score = calculate_score(combo, ingredients)
        best_score = max(best_score, score)
    
    return best_score

ingredients = parse_input('input.txt')
print("Part 1:", solve(ingredients))
print("Part 2:", solve(ingredients, calorie_limit=500))

from itertools import combinations_with_replacement

def parse_ingredients(input_data):
    ingredients = {}
    for line in input_data.strip().split('\n'):
        name, properties = line.split(': ')
        properties = properties.split(', ')
        ingredients[name] = {prop.split()[0]: int(prop.split()[1]) for prop in properties}
    return ingredients

def calculate_score(combination, ingredients):
    properties = {prop: 0 for prop in ['capacity', 'durability', 'flavor', 'texture']}
    for ingredient, amount in combination.items():
        for prop in properties:
            properties[prop] += ingredients[ingredient][prop] * amount
    
    score = 1
    for value in properties.values():
        score *= max(0, value)
    return score

def find_best_cookie(ingredients):
    max_score = 0
    ingredient_names = list(ingredients.keys())
    
    for combo in combinations_with_replacement(ingredient_names, 100):
        combination = {name: combo.count(name) for name in ingredient_names}
        if sum(combination.values()) == 100:
            score = calculate_score(combination, ingredients)
            max_score = max(max_score, score)
    
    return max_score

# Example input
input_data = """Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"""

ingredients = parse_ingredients(input_data)
best_score = find_best_cookie(ingredients)
print(best_score)

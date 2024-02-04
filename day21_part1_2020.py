with open('input.txt', 'r') as file:
    data = file.read().strip().split('\n')

allergens = {}
ingredients_count = {}

for food in data:
    ingredients, allergens_list = food.split(' (contains ')
    ingredients = ingredients.split()
    allergens_list = allergens_list[:-1].split(', ')
    
    for ingredient in ingredients:
        if ingredient not in ingredients_count:
            ingredients_count[ingredient] = 0
        ingredients_count[ingredient] += 1
    
    for allergen in allergens_list:
        if allergen not in allergens:
            allergens[allergen] = set(ingredients)
        else:
            allergens[allergen] = allergens[allergen].intersection(set(ingredients))

allergen_ingredients = set()
for ingredients in allergens.values():
    allergen_ingredients = allergen_ingredients.union(ingredients)

safe_ingredients_count = 0
for ingredient, count in ingredients_count.items():
    if ingredient not in allergen_ingredients:
        safe_ingredients_count += count

print(safe_ingredients_count)
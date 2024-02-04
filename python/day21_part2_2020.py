
import re

data = open("input.txt").read().strip().split("\n")

allergen_candidates = {}
ingredient_count = {}
all_ingredients = []

for line in data:
    ingredients, allergens = re.match(r"(.*) \(contains (.*)\)", line).groups()
    ingredients = ingredients.split()
    allergens = allergens.split(", ")

    for ingredient in ingredients:
        ingredient_count[ingredient] = ingredient_count.get(ingredient, 0) + 1

    all_ingredients.extend(ingredients)

    for allergen in allergens:
        if allergen not in allergen_candidates:
            allergen_candidates[allergen] = set(ingredients)
        else:
            allergen_candidates[allergen] = allergen_candidates[allergen].intersection(set(ingredients))

safe_ingredients = set(all_ingredients) - set.union(*allergen_candidates.values())
safe_count = sum(ingredient_count[ingredient] for ingredient in safe_ingredients)

print(safe_count)

dangerous_ingredients = {}
while allergen_candidates:
    for allergen, ingredients in allergen_candidates.items():
        if len(ingredients) == 1:
            ingredient = next(iter(ingredients))
            dangerous_ingredients[allergen] = ingredient
            del allergen_candidates[allergen]
            for other_ingredients in allergen_candidates.values():
                other_ingredients.discard(ingredient)
            break

canonical_dangerous_ingredients = ",".join(dangerous_ingredients[allergen] for allergen in sorted(dangerous_ingredients))
print(canonical_dangerous_ingredients)

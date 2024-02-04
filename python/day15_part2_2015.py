
class Ingredient:
    def __init__(self, name, capacity, durability, flavor, texture, calories):
        self.name = name
        self.capacity = capacity
        self.durability = durability
        self.flavor = flavor
        self.texture = texture
        self.calories = calories

def read_ingredients(filename):
    ingredients = []
    with open(filename, 'r') as file:
        for line in file:
            parts = line.split()
            if len(parts) < 11:
                continue  # Invalid line
            capacity = int(parts[2][:-1])
            durability = int(parts[4][:-1])
            flavor = int(parts[6][:-1])
            texture = int(parts[8][:-1])
            calories = int(parts[10])
            ingredients.append(Ingredient(parts[0], capacity, durability, flavor, texture, calories))
    return ingredients

def find_max_score(ingredients, total_teaspoons, target_calories):
    return calculate_max_score(ingredients, 0, total_teaspoons, [], target_calories)

def calculate_max_score(ingredients, index, remaining, teaspoons, target_calories):
    if index == len(ingredients) - 1:
        teaspoons.append(remaining)
        if calculate_calories(ingredients, teaspoons) == target_calories:
            return score(ingredients, teaspoons)
        return 0

    max_score = 0
    for i in range(remaining + 1):
        new_teaspoons = teaspoons + [i]
        current_score = calculate_max_score(ingredients, index + 1, remaining - i, new_teaspoons, target_calories)
        if current_score > max_score:
            max_score = current_score
    return max_score

def score(ingredients, teaspoons):
    capacity = durability = flavor = texture = 0
    for i, ingredient in enumerate(ingredients):
        capacity += ingredient.capacity * teaspoons[i]
        durability += ingredient.durability * teaspoons[i]
        flavor += ingredient.flavor * teaspoons[i]
        texture += ingredient.texture * teaspoons[i]

    if capacity < 0: capacity = 0
    if durability < 0: durability = 0
    if flavor < 0: flavor = 0
    if texture < 0: texture = 0

    return capacity * durability * flavor * texture

def calculate_calories(ingredients, teaspoons):
    calories = 0
    for i, ingredient in enumerate(ingredients):
        calories += ingredient.calories * teaspoons[i]
    return calories

if __name__ == "__main__":
    ingredients = read_ingredients("input.txt")
    max_score = find_max_score(ingredients, 100, 500)
    print(max_score)

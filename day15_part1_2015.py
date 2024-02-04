
import sys

class Ingredient:
    def __init__(self, name, capacity, durability, flavor, texture):
        self.name = name
        self.capacity = capacity
        self.durability = durability
        self.flavor = flavor
        self.texture = texture

def read_ingredients(filename):
    ingredients = []
    with open(filename, 'r') as file:
        for line in file:
            parts = line.split()
            if len(parts) < 11:
                continue

            capacity = int(parts[2][:-1])
            durability = int(parts[4][:-1])
            flavor = int(parts[6][:-1])
            texture = int(parts[8][:-1])

            ingredients.append(Ingredient(parts[0], capacity, durability, flavor, texture))

    return ingredients

def find_max_score(ingredients, total_teaspoons):
    return calculate_max_score(ingredients, 0, total_teaspoons, [])

def calculate_max_score(ingredients, index, remaining, teaspoons):
    if index == len(ingredients)-1:
        teaspoons.append(remaining)
        return score(ingredients, teaspoons)

    max_score = 0
    for i in range(remaining+1):
        score_val = calculate_max_score(ingredients, index+1, remaining-i, teaspoons + [i])
        max_score = max(max_score, score_val)

    return max_score

def score(ingredients, teaspoons):
    capacity, durability, flavor, texture = 0, 0, 0, 0
    for i, ingredient in enumerate(ingredients):
        capacity += ingredient.capacity * teaspoons[i]
        durability += ingredient.durability * teaspoons[i]
        flavor += ingredient.flavor * teaspoons[i]
        texture += ingredient.texture * teaspoons[i]

    capacity = max(0, capacity)
    durability = max(0, durability)
    flavor = max(0, flavor)
    texture = max(0, texture)

    return capacity * durability * flavor * texture

if __name__ == "__main__":
    ingredients = read_ingredients("input.txt")
    max_score = find_max_score(ingredients, 100)
    print(max_score)

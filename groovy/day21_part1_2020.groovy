def input = new File('input.txt').readLines()

def allergenMap = [:]
def ingredientCount = [:]

input.each { line ->
    def (ingredients, allergens) = line.split(" \\(contains ")
    ingredients = ingredients.tokenize()
    allergens = allergens[0..-2].tokenize(", ")

    ingredients.each { ingredient ->
        ingredientCount[ingredient] = ingredientCount.containsKey(ingredient) ? ingredientCount[ingredient] + 1 : 1
    }

    allergens.each { allergen ->
        if (allergenMap.containsKey(allergen)) {
            allergenMap[allergen] = allergenMap[allergen].intersect(ingredients as Set)
        } else {
            allergenMap[allergen] = ingredients as Set
        }
    }
}

def safeIngredients = ingredientCount.findAll { ingredient, count ->
    !allergenMap.any { it.value.contains(ingredient) }
}

def answer = safeIngredients.values().sum()

println answer
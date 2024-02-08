
def input = new File("input.txt").readLines()

def allergenMap = [:]
def ingredientCount = [:]

input.each { line ->
    def parts = line.split(" \\(contains ")
    def ingredients = parts[0].split(" ")
    def allergens = parts[1][0..-2].split(", ")

    ingredients.each { ingredient ->
        ingredientCount[ingredient] = ingredientCount.getOrDefault(ingredient, 0) + 1
    }

    allergens.each { allergen ->
        if (allergenMap.containsKey(allergen)) {
            allergenMap[allergen] = allergenMap[allergen].intersect(ingredients as Set)
        } else {
            allergenMap[allergen] = ingredients as Set
        }
    }
}

def inertIngredients = ingredientCount.findAll { entry -> !allergenMap.any { it.value.contains(entry.key) } }.keySet()

def part1 = input.sum { line -> line.split(" \\(contains ")[0].split(" ") }.count { it in inertIngredients }
println part1

def dangerousIngredients = [:]
while (allergenMap.any { it.value.size() > 1 }) {
    allergenMap.each { allergen, ingredients ->
        if (ingredients.size() == 1) {
            def ingredient = ingredients.first()
            dangerousIngredients[allergen] = ingredient
            allergenMap.each { key, value ->
                if (value.size() > 1) {
                    allergenMap[key] = value - ingredient
                }
            }
        }
    }
}

def part2 = dangerousIngredients.entrySet().toList().sort { it.key }.collect { it.value }.join(",")
println part2

class Ingredient {
    String name
    int capacity
    int durability
    int flavor
    int texture
}

def readIngredients(filename) {
    def ingredients = []
    new File(filename).eachLine { line ->
        def parts = line.split()
        if (parts.size() < 11) {
            return
        }

        def capacity = parts[2].minus(parts[2][-1]) as int
        def durability = parts[4].minus(parts[4][-1]) as int
        def flavor = parts[6].minus(parts[6][-1]) as int
        def texture = parts[8].minus(parts[8][-1]) as int

        ingredients << new Ingredient(name: parts[0], capacity: capacity, durability: durability, flavor: flavor, texture: texture)
    }
    ingredients
}

def findMaxScore(ingredients, totalTeaspoons) {
    calculateMaxScore(ingredients, 0, totalTeaspoons, [])
}

def calculateMaxScore(ingredients, index, remaining, teaspoons) {
    if (index == ingredients.size() - 1) {
        teaspoons << remaining
        score(ingredients, teaspoons)
    } else {
        def maxScore = 0
        (0..remaining).each { i ->
            def score = calculateMaxScore(ingredients, index + 1, remaining - i, teaspoons + [i])
            if (score > maxScore) {
                maxScore = score
            }
        }
        maxScore
    }
}

def score(ingredients, teaspoons) {
    def capacity = 0
    def durability = 0
    def flavor = 0
    def texture = 0
    (0..<ingredients.size()).each { i ->
        capacity += ingredients[i].capacity * teaspoons[i]
        durability += ingredients[i].durability * teaspoons[i]
        flavor += ingredients[i].flavor * teaspoons[i]
        texture += ingredients[i].texture * teaspoons[i]
    }

    capacity = Math.max(0, capacity)
    durability = Math.max(0, durability)
    flavor = Math.max(0, flavor)
    texture = Math.max(0, texture)

    capacity * durability * flavor * texture
}

def ingredients = readIngredients("input.txt")
def maxScore = findMaxScore(ingredients, 100)
println maxScore
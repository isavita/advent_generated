
import Foundation

struct Ingredient {
    let name: String
    let capacity: Int
    let durability: Int
    let flavor: Int
    let texture: Int
    let calories: Int
}

func readIngredients(from filename: String) -> [Ingredient] {
    let fileURL = URL(fileURLWithPath: filename)
    guard let input = try? String(contentsOf: fileURL) else { return [] }

    var ingredients: [Ingredient] = []
    let lines = input.components(separatedBy: .newlines)
    for line in lines {
        let parts = line.components(separatedBy: " ")
        if parts.count < 11 { continue }

        let capacity = Int(parts[2].dropLast()) ?? 0
        let durability = Int(parts[4].dropLast()) ?? 0
        let flavor = Int(parts[6].dropLast()) ?? 0
        let texture = Int(parts[8].dropLast()) ?? 0
        let calories = Int(parts[10]) ?? 0

        let ingredient = Ingredient(name: parts[0], capacity: capacity, durability: durability, flavor: flavor, texture: texture, calories: calories)
        ingredients.append(ingredient)
    }

    return ingredients
}

func findMaxScore(ingredients: [Ingredient], totalTeaspoons: Int, targetCalories: Int) -> Int {
    return calculateMaxScore(ingredients: ingredients, index: 0, remaining: totalTeaspoons, teaspoons: [], targetCalories: targetCalories)
}

func calculateMaxScore(ingredients: [Ingredient], index: Int, remaining: Int, teaspoons: [Int], targetCalories: Int) -> Int {
    if index == ingredients.count - 1 {
        var updatedTeaspoons = teaspoons
        updatedTeaspoons.append(remaining)
        if calculateCalories(ingredients: ingredients, teaspoons: updatedTeaspoons) == targetCalories {
            return score(ingredients: ingredients, teaspoons: updatedTeaspoons)
        }
        return 0
    }

    var maxScore = 0
    for i in 0...remaining {
        let score = calculateMaxScore(ingredients: ingredients, index: index + 1, remaining: remaining - i, teaspoons: teaspoons + [i], targetCalories: targetCalories)
        if score > maxScore {
            maxScore = score
        }
    }
    return maxScore
}

func score(ingredients: [Ingredient], teaspoons: [Int]) -> Int {
    var capacity = 0
    var durability = 0
    var flavor = 0
    var texture = 0

    for i in 0..<ingredients.count {
        capacity += ingredients[i].capacity * teaspoons[i]
        durability += ingredients[i].durability * teaspoons[i]
        flavor += ingredients[i].flavor * teaspoons[i]
        texture += ingredients[i].texture * teaspoons[i]
    }

    if capacity < 0 { capacity = 0 }
    if durability < 0 { durability = 0 }
    if flavor < 0 { flavor = 0 }
    if texture < 0 { texture = 0 }

    return capacity * durability * flavor * texture
}

func calculateCalories(ingredients: [Ingredient], teaspoons: [Int]) -> Int {
    var calories = 0
    for i in 0..<ingredients.count {
        calories += ingredients[i].calories * teaspoons[i]
    }
    return calories
}

let ingredients = readIngredients(from: "input.txt")
let maxScore = findMaxScore(ingredients: ingredients, totalTeaspoons: 100, targetCalories: 500)
print(maxScore)

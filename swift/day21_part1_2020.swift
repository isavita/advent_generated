
import Foundation

let input = try String(contentsOfFile: "input.txt")
let lines = input.components(separatedBy: .newlines)

var allergenMap: [String: Set<String>] = [:]
var ingredientCount: [String: Int] = [:]

for line in lines {
    let parts = line.components(separatedBy: " (contains ")
    let ingredients = Set(parts[0].components(separatedBy: " "))
    let allergens = Set(parts[1].replacingOccurrences(of: ")", with: "").components(separatedBy: ", "))

    for allergen in allergens {
        if let existingIngredients = allergenMap[allergen] {
            allergenMap[allergen] = existingIngredients.intersection(ingredients)
        } else {
            allergenMap[allergen] = ingredients
        }
    }

    for ingredient in ingredients {
        ingredientCount[ingredient, default: 0] += 1
    }
}

let allergenIngredients = Set(allergenMap.values.joined())
let safeIngredients = Set(ingredientCount.keys).subtracting(allergenIngredients)

let answer = safeIngredients.reduce(0) { $0 + (ingredientCount[$1] ?? 0) }
print(answer)

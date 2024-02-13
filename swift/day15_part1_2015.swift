
import Foundation

struct Ingredient {
    let name: String
    let capacity: Int
    let durability: Int
    let flavor: Int
    let texture: Int
}

func readIngredients(from file: String) -> [Ingredient] {
    guard let contents = try? String(contentsOfFile: file) else { return [] }
    
    var ingredients: [Ingredient] = []
    let lines = contents.components(separatedBy: .newlines)
    
    for line in lines {
        let parts = line.components(separatedBy: " ")
        if parts.count < 11 { continue }
        
        let capacity = Int(parts[2].dropLast()) ?? 0
        let durability = Int(parts[4].dropLast()) ?? 0
        let flavor = Int(parts[6].dropLast()) ?? 0
        let texture = Int(parts[8].dropLast()) ?? 0
        
        ingredients.append(Ingredient(name: parts[0], capacity: capacity, durability: durability, flavor: flavor, texture: texture))
    }
    
    return ingredients
}

func findMaxScore(_ ingredients: [Ingredient], _ totalTeaspoons: Int) -> Int {
    return calculateMaxScore(ingredients, 0, totalTeaspoons, [])
}

func calculateMaxScore(_ ingredients: [Ingredient], _ index: Int, _ remaining: Int, _ teaspoons: [Int]) -> Int {
    if index == ingredients.count - 1 {
        var updatedTeaspoons = teaspoons
        updatedTeaspoons.append(remaining)
        return score(ingredients, updatedTeaspoons)
    }
    
    var maxScore = 0
    for i in 0...remaining {
        let score = calculateMaxScore(ingredients, index + 1, remaining - i, teaspoons + [i])
        if score > maxScore {
            maxScore = score
        }
    }
    return maxScore
}

func score(_ ingredients: [Ingredient], _ teaspoons: [Int]) -> Int {
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

let ingredients = readIngredients(from: "input.txt")
let maxScore = findMaxScore(ingredients, 100)
print(maxScore)

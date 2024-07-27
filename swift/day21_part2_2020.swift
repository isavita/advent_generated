
import Foundation

func readInput(from filename: String) -> [String] {
    do {
        let contents = try String(contentsOfFile: filename)
        return contents.components(separatedBy: .newlines).filter { !$0.isEmpty }
    } catch {
        print("Error reading file: \(error)")
        return []
    }
}

func parseFoodData(_ foodData: [String]) -> (ingredients: [String: Int], allergens: [String: Set<String>]) {
    var ingredientCount: [String: Int] = [:]
    var allergenMap: [String: Set<String>] = [:]
    
    for line in foodData {
        let components = line.components(separatedBy: " (contains ")
        let ingredients = components[0].split(separator: " ").map(String.init)
        let allergens = components.count > 1 ? components[1].dropLast().split(separator: ", ").map(String.init) : []
        
        // Count ingredients
        for ingredient in ingredients {
            ingredientCount[ingredient, default: 0] += 1
        }
        
        // Map allergens to ingredients
        for allergen in allergens {
            if allergenMap[allergen] == nil {
                allergenMap[allergen] = Set(ingredients)
            } else {
                allergenMap[allergen]!.formIntersection(Set(ingredients))
            }
        }
    }
    
    return (ingredientCount, allergenMap)
}

func findSafeIngredients(ingredientCount: [String: Int], allergenMap: [String: Set<String>]) -> Set<String> {
    var unsafeIngredients = Set<String>()
    
    for ingredients in allergenMap.values {
        unsafeIngredients.formUnion(ingredients)
    }
    
    return Set(ingredientCount.keys).subtracting(unsafeIngredients)
}

func countSafeIngredientOccurrences(ingredientCount: [String: Int], safeIngredients: Set<String>) -> Int {
    return safeIngredients.reduce(0) { $0 + (ingredientCount[$1] ?? 0) }
}

func determineCanonicalDangerousIngredientList(allergenMap: [String: Set<String>]) -> String {
    var allergenToIngredient: [String: String] = [:]
    var resolvedAllergens: Set<String> = []
    
    while allergenToIngredient.count < allergenMap.count {
        for (allergen, ingredients) in allergenMap where allergenToIngredient[allergen] == nil {
            let unresolvedIngredients = ingredients.subtracting(Set(resolvedAllergens))
            if unresolvedIngredients.count == 1 {
                let ingredient = unresolvedIngredients.first!
                allergenToIngredient[allergen] = ingredient
                resolvedAllergens.insert(ingredient)
            }
        }
    }
    
    return allergenToIngredient.sorted(by: { $0.key < $1.key }).map { $0.value }.joined(separator: ",")
}

func main() {
    let foodData = readInput(from: "input.txt")
    let (ingredientCount, allergenMap) = parseFoodData(foodData)
    
    let safeIngredients = findSafeIngredients(ingredientCount: ingredientCount, allergenMap: allergenMap)
    let safeIngredientCount = countSafeIngredientOccurrences(ingredientCount: ingredientCount, safeIngredients: safeIngredients)
    
    print("Safe ingredient occurrences: \(safeIngredientCount)")
    
    let canonicalDangerousIngredientList = determineCanonicalDangerousIngredientList(allergenMap: allergenMap)
    print("Canonical dangerous ingredient list: \(canonicalDangerousIngredientList)")
}

main()

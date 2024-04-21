import Foundation

struct Chemical {
    let name: String
    let amount: Int
}

func parseChemical(_ s: String) -> Chemical {
    let parts = s.components(separatedBy: " ")
    guard let amount = Int(parts[0]) else { fatalError() }
    return Chemical(name: String(parts[1...].joined(separator: " ")), amount: amount)
}

func readInput(from file: String) -> ([String: Chemical], [String: [Chemical]]) {
    let fileURL = URL(fileURLWithPath: file)
    do {
        let content = try String(contentsOf: fileURL, encoding: .utf8)
        let lines = content.components(separatedBy: "\n")
        var reactions: [String: Chemical] = [:]
        var ingredients: [String: [Chemical]] = [:]
        for line in lines {
            let parts = line.components(separatedBy: " => ")
            let output = parseChemical(parts[1])
            let inputs = parts[0].components(separatedBy: ", ").map { parseChemical($0) }
            reactions[output.name] = output
            ingredients[output.name] = inputs
        }
        return (reactions, ingredients)
    } catch {
        fatalError("Error reading file: \(error)")
    }
}

func calculateOre(_ chem: String, amount: Int, reactions: [String: Chemical], ingredients: [String: [Chemical]], surplus: inout [String: Int]) -> Int {
    if chem == "ORE" {
        return amount
    }
    
    if let available = surplus[chem], available >= amount {
        surplus[chem] = available - amount
        return 0
    }
    
    var remainingAmount = amount
    if let available = surplus[chem] {
        remainingAmount -= available
        surplus[chem] = 0
    }
    
    guard let reaction = reactions[chem] else { fatalError() }
    let times = (remainingAmount + reaction.amount - 1) / reaction.amount
    var ore = 0
    
    for ingredient in ingredients[chem, default: []] {
        ore += calculateOre(ingredient.name, amount: ingredient.amount * times, reactions: reactions, ingredients: ingredients, surplus: &surplus)
    }
    
    surplus[chem] = times * reaction.amount - remainingAmount
    return ore
}

func maxFuel(reactions: [String: Chemical], ingredients: [String: [Chemical]]) -> Int64 {
    let oreAvailable: Int64 = 1000000000000
    var low: Int64 = 0
    var high = oreAvailable
    var surplus: [String: Int] = [:]
    while low < high {
        let mid = (low + high + 1) / 2
        surplus.removeAll()
        let ore = calculateOre("FUEL", amount: Int(mid), reactions: reactions, ingredients: ingredients, surplus: &surplus)
        if Int64(ore) > oreAvailable {
            high = mid - 1
        } else {
            low = mid
        }
    }
    return low
}

let (reactions, ingredients) = readInput(from: "input.txt")
print(maxFuel(reactions: reactions, ingredients: ingredients))
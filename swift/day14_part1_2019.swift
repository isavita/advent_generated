
import Foundation

struct Reaction {
    let inputs: [String: Int]
    let output: (String, Int)
}

func parseReactions(from file: String) -> [String: Reaction] {
    var reactions: [String: Reaction] = [:]
    
    let lines = file.components(separatedBy: .newlines).filter { !$0.isEmpty }
    for line in lines {
        let parts = line.components(separatedBy: " => ")
        let inputs = parts[0].components(separatedBy: ", ")
        let output = parts[1].components(separatedBy: " ")
        
        var inputDict: [String: Int] = [:]
        for input in inputs {
            let components = input.components(separatedBy: " ")
            let quantity = Int(components[0])!
            let chemical = components[1]
            inputDict[chemical] = quantity
        }
        
        let outputQuantity = Int(output[0])!
        let outputChemical = output[1]
        reactions[outputChemical] = Reaction(inputs: inputDict, output: (outputChemical, outputQuantity))
    }
    
    return reactions
}

func calculateOreNeeded(forFuel fuelAmount: Int, reactions: [String: Reaction]) -> Int {
    var neededChemicals: [String: Int] = ["FUEL": fuelAmount]
    var excessChemicals: [String: Int] = [:]
    var oreNeeded = 0
    
    while !neededChemicals.isEmpty {
        let chemical = neededChemicals.keys.first!
        let neededAmount = neededChemicals.removeValue(forKey: chemical)!
        
        if chemical == "ORE" {
            oreNeeded += neededAmount
            continue
        }
        
        if let excess = excessChemicals[chemical], excess >= neededAmount {
            excessChemicals[chemical] = excess - neededAmount
            continue
        }
        
        let available = excessChemicals[chemical] ?? 0
        let amountToProduce = neededAmount - available
        excessChemicals[chemical] = 0
        
        let reaction = reactions[chemical]!
        let reactionOutputQuantity = reaction.output.1
        let multiplier = (amountToProduce + reactionOutputQuantity - 1) / reactionOutputQuantity
        
        let producedAmount = multiplier * reactionOutputQuantity
        let excess = producedAmount - amountToProduce
        if excess > 0 {
            excessChemicals[chemical] = excess
        }
        
        for (inputChemical, inputQuantity) in reaction.inputs {
            let totalInputNeeded = inputQuantity * multiplier
            neededChemicals[inputChemical, default: 0] += totalInputNeeded
        }
    }
    
    return oreNeeded
}

func main() {
    do {
        let input = try String(contentsOfFile: "input.txt", encoding: .utf8)
        let reactions = parseReactions(from: input)
        let oreNeeded = calculateOreNeeded(forFuel: 1, reactions: reactions)
        print(oreNeeded)
        
    } catch {
        print("Error reading file: \(error)")
    }
}

main()

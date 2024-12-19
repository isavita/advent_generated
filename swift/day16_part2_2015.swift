
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)

let targetSue: [String: Int] = [
    "children": 3,
    "cats": 7,
    "samoyeds": 2,
    "pomeranians": 3,
    "akitas": 0,
    "vizslas": 0,
    "goldfish": 5,
    "trees": 3,
    "cars": 2,
    "perfumes": 1
]

func auntSue(input: String) -> Int {
    let lines = input.split(separator: "\n")
    for line in lines {
        let components = line.split(separator: " ")
        let sueNum = Int(components[1].dropLast())!
        var readings: [String: Int] = [:]
        
        for i in stride(from: 2, to: components.count, by: 2) {
            let key = String(components[i].dropLast())
            let value = Int(components[i+1].replacingOccurrences(of: ",", with: ""))!
            readings[key] = value
        }

        var allRulesMatched = true
        for (thing, amount) in readings {
            if let targetAmount = targetSue[thing] {
                if (thing == "cats" || thing == "trees") && amount <= targetAmount {
                    allRulesMatched = false
                    break
                } else if (thing == "pomeranians" || thing == "goldfish") && amount >= targetAmount {
                    allRulesMatched = false
                    break
                } else if thing != "cats" && thing != "trees" && thing != "pomeranians" && thing != "goldfish" && amount != targetAmount {
                    allRulesMatched = false
                    break
                }
            }
        }

        if allRulesMatched {
            return sueNum
        }
    }

    fatalError("expect return from loop")
}

let res = auntSue(input: input)
print(res)

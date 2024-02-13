
import Foundation

let rules = try String(contentsOfFile: "input.txt").components(separatedBy: .newlines)

var bagRules: [String: [(Int, String)]] = [:]

for rule in rules {
    let components = rule.components(separatedBy: " bags contain ")
    let bagColor = components[0]
    let contents = components[1].components(separatedBy: ", ")
    
    var containedBags: [(Int, String)] = []
    
    for content in contents {
        if content != "no other bags." {
            let contentComponents = content.components(separatedBy: " ")
            let count = Int(contentComponents[0]) ?? 0
            let color = "\(contentComponents[1]) \(contentComponents[2])"
            containedBags.append((count, color))
        }
    }
    
    bagRules[bagColor] = containedBags
}

func containsShinyGoldBag(color: String) -> Bool {
    if color == "shiny gold" {
        return true
    }
    
    for (_, innerColor) in bagRules[color] ?? [] {
        if containsShinyGoldBag(color: innerColor) {
            return true
        }
    }
    
    return false
}

func countInnerBags(color: String) -> Int {
    var count = 0
    
    for (innerCount, innerColor) in bagRules[color] ?? [] {
        count += innerCount + innerCount * countInnerBags(color: innerColor)
    }
    
    return count
}

var part1Count = 0
var part2Count = 0

for (color, _) in bagRules {
    if containsShinyGoldBag(color: color) {
        part1Count += 1
    }
}

part2Count = countInnerBags(color: "shiny gold")

print(part1Count)
print(part2Count)

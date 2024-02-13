
import Foundation

let input = try String(contentsOfFile: "input.txt")
let rules = input.components(separatedBy: "\n")

var bagMap = [String: [String]]()

for rule in rules {
    let components = rule.components(separatedBy: " bags contain ")
    let bagColor = components[0]
    let contents = components[1].components(separatedBy: ", ")

    for content in contents {
        if content != "no other bags." {
            let contentComponents = content.components(separatedBy: " ")
            let color = contentComponents[1] + " " + contentComponents[2]
            if let bags = bagMap[color] {
                bagMap[color] = bags + [bagColor]
            } else {
                bagMap[color] = [bagColor]
            }
        }
    }
}

func findContainingBags(color: String, map: [String: [String]]) -> Set<String> {
    var result = Set<String>()
    if let bags = map[color] {
        for bag in bags {
            result.insert(bag)
            result.formUnion(findContainingBags(color: bag, map: map))
        }
    }
    return result
}

let containingBags = findContainingBags(color: "shiny gold", map: bagMap)
print(containingBags.count)

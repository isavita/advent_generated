
import Foundation

let input = try String(contentsOfFile: "input.txt")
let components = input.components(separatedBy: "\n\n")
let rules = components[0].components(separatedBy: "\n").map { $0.components(separatedBy: ": ") }
let messages = components[1].components(separatedBy: "\n")

var ruleMap = [String: String]()

for rule in rules {
    ruleMap[rule[0]] = rule[1]
}

func evaluateRule(_ rule: String) -> String {
    if rule.contains("\"") {
        return rule.replacingOccurrences(of: "\"", with: "")
    }
    
    let subRules = rule.components(separatedBy: " | ")
    
    var result = "("
    
    for (index, subRule) in subRules.enumerated() {
        let ruleIds = subRule.components(separatedBy: " ")
        
        for ruleId in ruleIds {
            result += evaluateRule(ruleMap[ruleId]!)
        }
        
        if index < subRules.count - 1 {
            result += "|"
        }
    }
    
    result += ")"
    
    return result
}

let regex = "^" + evaluateRule(ruleMap["0"]!) + "$"

let validMessages = messages.filter { message in
    let range = NSRange(location: 0, length: message.utf16.count)
    let regex = try! NSRegularExpression(pattern: regex)
    return regex.firstMatch(in: message, options: [], range: range) != nil
}

print(validMessages.count)

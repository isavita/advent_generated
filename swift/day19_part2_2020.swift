import Foundation
import RegexBuilder

struct Rule {
    var resolved: [String] = []
    var options: [[Int]] = []
}

func fillInGraph(_ graph: inout [Int: Rule], _ entry: Int) -> [String] {
    if !graph[entry]!.resolved.isEmpty {
        return graph[entry]!.resolved
    }

    for option in graph[entry]!.options {
        var resolved: [String] = [""]
        for entryPoint in option {
            let nestedResolveVals = fillInGraph(&graph, entryPoint)
            var newResolved: [String] = []
            for nextPiece in nestedResolveVals {
                for prev in resolved {
                    newResolved.append(prev + nextPiece)
                }
            }
            resolved = newResolved
        }
        graph[entry]!.resolved.append(contentsOf: resolved)
    }

    return graph[entry]!.resolved
}

func parseInput(_ input: String) -> ([Int: Rule], [String]) {
    let parts = input.components(separatedBy: "\n\n")
    var rules: [Int: Rule] = [:]
    var messages: [String] = []

    for r in parts[0].components(separatedBy: "\n") {
        if let match = r.range(of: "[a-z]+", options: .regularExpression) {
            let num = Int(r.components(separatedBy: ": ")[0])!
            let char = String(r[match])
            rules[num] = Rule(resolved: [char])
        } else {
            let split = r.components(separatedBy: ": ")
            let key = Int(split[0])!
            var newRule = Rule()
            for ruleNums in split[1].components(separatedBy: " | ") {
                var option: [Int] = []
                for n in ruleNums.components(separatedBy: " ") {
                    option.append(Int(n)!)
                }
                newRule.options.append(option)
            }
            rules[key] = newRule
        }
    }

    messages = parts[1].components(separatedBy: "\n")

    return (rules, messages)
}

func solve(_ input: String) -> Int {
    var graph: [Int: Rule] = [:]
    var messages: [String] = []
    (graph, messages) = parseInput(input)

    _ = fillInGraph(&graph, 42)
    _ = fillInGraph(&graph, 31)

    let part42 = "(\(graph[42]!.resolved.joined(separator: "|")))"
    let part31 = "(\(graph[31]!.resolved.joined(separator: "|")))"
    let rule8String = "(\(part42))+"

    var matchRuleZero = 0
    for m in messages {
        for i in 1...10 {
            let pattern = try! NSRegularExpression(pattern: "^" + rule8String + String(repeating: part42, count: i) + String(repeating: part31, count: i) + "$")
            if pattern.numberOfMatches(in: m, options: [], range: NSRange(location: 0, length: m.count)) > 0 {
                matchRuleZero += 1
                break
            }
        }
    }

    return matchRuleZero
}

let fileContent = try! String(contentsOfFile: "input.txt")
let result = solve(fileContent)
print(result)
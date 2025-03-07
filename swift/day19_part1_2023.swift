
import Foundation

struct Rule {
    let category: Character?
    let operation: Character?
    let value: Int?
    let destination: String

    func applies(to part: [Character: Int]) -> Bool {
        guard let category = category, let operation = operation, let value = value else {
            return true // Default rule (always applies)
        }

        guard let partValue = part[category] else {
            return false // Part doesn't have this category
        }

        switch operation {
        case "<":
            return partValue < value
        case ">":
            return partValue > value
        default:
            return false // Invalid operation
        }
    }
}

struct Workflow {
    let name: String
    let rules: [Rule]

    func process(part: [Character: Int]) -> String {
        for rule in rules {
            if rule.applies(to: part) {
                return rule.destination
            }
        }
        return "R" // Should not reach here as last rule is catch all.
    }
}

func parseInput(from fileURL: URL) -> ([Workflow], [[Character: Int]])? {
    do {
        let input = try String(contentsOf: fileURL)
        let sections = input.components(separatedBy: "\n\n")
        
        guard sections.count == 2 else { return nil }

        let workflowLines = sections[0].components(separatedBy: .newlines)
        let partLines = sections[1].components(separatedBy: .newlines)

        var workflows: [Workflow] = []
        var parts: [[Character: Int]] = []

        for line in workflowLines {
            if line.isEmpty { continue }
            let name = String(line.prefix(while: { $0 != "{" }))
            let ruleString = line.dropFirst(name.count + 1).dropLast(1)
            let ruleStrings = ruleString.components(separatedBy: ",")
            
            var rules: [Rule] = []
            for ruleStr in ruleStrings {
                let components = ruleStr.components(separatedBy: ":")
                if components.count == 2 {
                    let condition = components[0]
                    let destination = components[1]
                    if let opIndex = condition.firstIndex(where: { $0 == "<" || $0 == ">" }) {
                        let category = condition[condition.startIndex]
                        let operation = condition[opIndex]
                        let value = Int(condition.suffix(from: condition.index(after: opIndex)))!
                        rules.append(Rule(category: category, operation: operation, value: value, destination: destination))
                    } else {
                        print("Error condition component count not equal 2")
                        
                    }
                    
                } else if components.count == 1 {
                    // Last rule is else.
                    rules.append(Rule(category: nil, operation: nil, value: nil, destination: components[0]))
                } else {
                    print("Error rule component count not equal 1 or 2")
                }
            }
            workflows.append(Workflow(name: name, rules: rules))
        }

        for line in partLines {
            if line.isEmpty { continue }
            
            let cleanLine = line.dropFirst().dropLast()
            let keyValuePairs = cleanLine.components(separatedBy: ",")
            var part: [Character: Int] = [:]
            for pair in keyValuePairs {
                let keyValue = pair.components(separatedBy: "=")
                if keyValue.count == 2, let value = Int(keyValue[1]) {
                    part[Character(keyValue[0])] = value
                } else {
                    print("Error parsing part data.")
                }
            }
            parts.append(part)
        }

        return (workflows, parts)

    } catch {
        print("Error reading file: \(error)")
        return nil
    }
}

func solve() {
    let fileURL = URL(fileURLWithPath: "input.txt")
    guard let (workflows, parts) = parseInput(from: fileURL) else {
        print("Failed to parse input.")
        return
    }

    var workflowMap: [String: Workflow] = [:]
    for workflow in workflows {
        workflowMap[workflow.name] = workflow
    }

    var totalRating = 0
    for part in parts {
        var currentWorkflow = "in"
        while currentWorkflow != "A" && currentWorkflow != "R" {
            if let workflow = workflowMap[currentWorkflow] {
                currentWorkflow = workflow.process(part: part)
            } else {
                print("workflow error") //shouldnt get here
                return
            }
        }

        if currentWorkflow == "A" {
            totalRating += part.values.reduce(0, +)
        }
    }

    print(totalRating)
}

// Proper main entry point
solve()


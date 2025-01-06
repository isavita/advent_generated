
import Foundation

struct Part {
    var x: Int
    var m: Int
    var a: Int
    var s: Int
}

enum Destination {
    case workflow(String)
    case accept
    case reject
}

struct Rule {
    let category: Character?
    let comparison: Character?
    let value: Int?
    let destination: Destination
}

struct Workflow {
    let name: String
    let rules: [Rule]
}

func parseInput(from fileURL: URL) -> ([String: Workflow], [Part]) {
    guard let input = try? String(contentsOf: fileURL) else {
        fatalError("Failed to read input file")
    }
    
    var workflows: [String: Workflow] = [:]
    var parts: [Part] = []
    
    var parsingWorkflows = true
    
    for line in input.components(separatedBy: .newlines) {
        if line.isEmpty {
            parsingWorkflows = false
            continue
        }
        
        if parsingWorkflows {
            let parts = line.components(separatedBy: "{")
            let name = parts[0]
            let rulesString = parts[1].dropLast()
            
            var rules: [Rule] = []
            for ruleString in rulesString.components(separatedBy: ",") {
                let ruleParts = ruleString.components(separatedBy: ":")
                if ruleParts.count == 1 {
                    let destination = ruleParts[0]
                    let dest: Destination
                    switch destination {
                    case "A":
                        dest = .accept
                    case "R":
                        dest = .reject
                    default:
                        dest = .workflow(destination)
                    }
                    rules.append(Rule(category: nil, comparison: nil, value: nil, destination: dest))
                } else {
                    let condition = ruleParts[0]
                    let destination = ruleParts[1]
                    let category = condition.first!
                    let comparison = condition.dropFirst().first!
                    let value = Int(condition.dropFirst(2))!
                    let dest: Destination
                    switch destination {
                    case "A":
                        dest = .accept
                    case "R":
                        dest = .reject
                    default:
                        dest = .workflow(destination)
                    }
                    rules.append(Rule(category: category, comparison: comparison, value: value, destination: dest))
                }
            }
            workflows[name] = Workflow(name: name, rules: rules)
        } else {
            let cleanLine = line.trimmingCharacters(in: CharacterSet(charactersIn: "{}"))
            let values = cleanLine.components(separatedBy: ",").map {
                Int($0.components(separatedBy: "=")[1])!
            }
            parts.append(Part(x: values[0], m: values[1], a: values[2], s: values[3]))
        }
    }
    
    return (workflows, parts)
}

func processPart(part: Part, workflows: [String: Workflow]) -> Destination {
    var currentWorkflow = "in"
    
    while true {
        guard let workflow = workflows[currentWorkflow] else {
            fatalError("Workflow \(currentWorkflow) not found")
        }
        
        for rule in workflow.rules {
            if let category = rule.category, let comparison = rule.comparison, let value = rule.value {
                let partValue: Int
                switch category {
                case "x":
                    partValue = part.x
                case "m":
                    partValue = part.m
                case "a":
                    partValue = part.a
                case "s":
                    partValue = part.s
                default:
                    fatalError("Invalid category: \(category)")
                }
                
                var conditionMet = false
                switch comparison {
                case "<":
                    conditionMet = partValue < value
                case ">":
                    conditionMet = partValue > value
                default:
                    fatalError("Invalid comparison: \(comparison)")
                }
                
                if conditionMet {
                    switch rule.destination {
                    case .accept:
                        return .accept
                    case .reject:
                        return .reject
                    case .workflow(let nextWorkflow):
                        currentWorkflow = nextWorkflow
                        break
                    }
                    break
                }
            } else {
                switch rule.destination {
                case .accept:
                    return .accept
                case .reject:
                    return .reject
                case .workflow(let nextWorkflow):
                    currentWorkflow = nextWorkflow
                    break
                }
            }
        }
    }
}

func part1(workflows: [String: Workflow], parts: [Part]) -> Int {
    var totalRating = 0
    for part in parts {
        let result = processPart(part: part, workflows: workflows)
        if case .accept = result {
            totalRating += part.x + part.m + part.a + part.s
        }
    }
    return totalRating
}

func part2(workflows: [String: Workflow]) -> Int {
    
    func countAcceptedCombinations(workflowName: String, ranges: [Character: ClosedRange<Int>]) -> Int {
        if workflowName == "A" {
            return ranges.values.reduce(1) { $0 * ($1.upperBound - $1.lowerBound + 1) }
        }
        if workflowName == "R" {
            return 0
        }
        
        guard let workflow = workflows[workflowName] else {
            fatalError("Workflow \(workflowName) not found")
        }
        
        var totalCombinations = 0
        var currentRanges = ranges
        
        for rule in workflow.rules {
            if let category = rule.category, let comparison = rule.comparison, let value = rule.value {
                var trueRanges = currentRanges
                var falseRanges = currentRanges
                
                let currentRange = currentRanges[category]!
                
                switch comparison {
                case "<":
                    trueRanges[category] = currentRange.lowerBound...min(value - 1, currentRange.upperBound)
                    falseRanges[category] = max(value, currentRange.lowerBound)...currentRange.upperBound
                case ">":
                    trueRanges[category] = max(value + 1, currentRange.lowerBound)...currentRange.upperBound
                    falseRanges[category] = currentRange.lowerBound...min(value, currentRange.upperBound)
                default:
                    fatalError("Invalid comparison: \(comparison)")
                }
                
                switch rule.destination {
                case .accept:
                    totalCombinations += countAcceptedCombinations(workflowName: "A", ranges: trueRanges)
                case .reject:
                    break
                case .workflow(let nextWorkflow):
                    totalCombinations += countAcceptedCombinations(workflowName: nextWorkflow, ranges: trueRanges)
                }
                currentRanges = falseRanges
            } else {
                switch rule.destination {
                case .accept:
                    totalCombinations += countAcceptedCombinations(workflowName: "A", ranges: currentRanges)
                case .reject:
                    break
                case .workflow(let nextWorkflow):
                    totalCombinations += countAcceptedCombinations(workflowName: nextWorkflow, ranges: currentRanges)
                }
            }
        }
        return totalCombinations
    }
    
    let initialRanges: [Character: ClosedRange<Int>] = [
        "x": 1...4000,
        "m": 1...4000,
        "a": 1...4000,
        "s": 1...4000
    ]
    
    return countAcceptedCombinations(workflowName: "in", ranges: initialRanges)
}

let fileURL = URL(fileURLWithPath: "input.txt")
let (workflows, parts) = parseInput(from: fileURL)

print("Part 1:", part1(workflows: workflows, parts: parts))
print("Part 2:", part2(workflows: workflows))

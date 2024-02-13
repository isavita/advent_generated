
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL).trimmingCharacters(in: .whitespacesAndNewlines)

var steps = [String: Set<String>]()

input.components(separatedBy: .newlines).forEach { line in
    let parts = line.components(separatedBy: " ")
    let step1 = parts[1]
    let step2 = parts[7]
    
    steps[step2, default: []].insert(step1)
    steps[step1, default: []] = steps[step1, default: []] // Fixed line to assign default value
}

var result = ""
while !steps.isEmpty {
    let nextStep = steps.filter { $0.value.isEmpty }.keys.sorted().first!
    result.append(nextStep)
    steps.removeValue(forKey: nextStep)
    steps.forEach { steps[$0.key]?.remove(nextStep) }
}

print(result)

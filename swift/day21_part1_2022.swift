
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)
let lines = input.components(separatedBy: .newlines)

var jobs = [String: String]()
var results = [String: Int]()

for line in lines {
    let parts = line.components(separatedBy: ": ")
    jobs[parts[0]] = parts[1]
}

func calculate(_ monkey: String, _ jobs: [String: String], _ results: inout [String: Int]) -> Int {
    if let val = results[monkey] {
        return val
    }

    guard let job = jobs[monkey] else {
        fatalError("Monkey not found: \(monkey)")
    }

    if let num = Int(job) {
        results[monkey] = num
        return num
    }

    let parts = job.components(separatedBy: " ")
    let a = calculate(parts[0], jobs, &results)
    let b = calculate(parts[2], jobs, &results)

    var result = 0
    switch parts[1] {
    case "+":
        result = a + b
    case "-":
        result = a - b
    case "*":
        result = a * b
    case "/":
        result = a / b
    default:
        fatalError("Unknown operation: \(parts[1])")
    }

    results[monkey] = result
    return result
}

print(calculate("root", jobs, &results))


import Foundation

struct Program {
    var weight: Int
    var holds: [String]
}

func dfs(name: String, programs: inout [String: Program]) -> (Int, Bool) {
    guard let program = programs[name] else { return (0, true) }
    var totalWeight = program.weight
    var weights = [Int: Int]()

    for child in program.holds {
        let (weight, balanced) = dfs(name: child, programs: &programs)
        if !balanced { return (0, false) }
        totalWeight += weight
        weights[weight, default: 0] += 1
    }

    for (w1, c1) in weights {
        for (w2, c2) in weights where w1 != w2 && c1 < c2 {
            let unbalancedProgram = program.holds.first { child in
                let (childWeight, _) = dfs(name: child, programs: &programs)
                return childWeight == w1
            }!
            print(programs[unbalancedProgram]!.weight + (w2 - w1))
            return (0, false)
        }
    }
    return (totalWeight, true)
}

func main() {
    guard let data = try? String(contentsOfFile: "input.txt") else { return }
    let lines = data.split(separator: "\n").map { String($0) }
    var programs = [String: Program]()

    let regex = try! NSRegularExpression(pattern: "[a-z]+|\\d+", options: [])

    for line in lines {
        let nsLine = line as NSString
        let matches = regex.matches(in: line, options: [], range: NSRange(location: 0, length: nsLine.length))
        let components = matches.map { nsLine.substring(with: $0.range) }
        let name = components[0]
        let weight = Int(components[1])!

        var program = Program(weight: weight, holds: [])
        if components.count > 2 {
            program.holds = Array(components[2...])
        }
        programs[name] = program
    }

    let root = "dtacyn" // Replace with the root found in Part One
    _ = dfs(name: root, programs: &programs)
}

main()

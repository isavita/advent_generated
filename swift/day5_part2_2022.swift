
import Foundation

func solve() {
    let s = try! String(contentsOfFile: "input.txt").components(separatedBy: "\n\n")
    let input = s[0].components(separatedBy: "\n")
    var stacks: [[Character]] = Array(repeating: [], count: (input[0].count + 1) / 4)
    for line in input {
        for (i, b) in line.enumerated() {
            if b >= "A" && b <= "Z" {
                stacks[(i - 1) / 4].insert(b, at: 0)
            }
        }
    }

    let steps = s[1].components(separatedBy: "\n")
    print(move(stacks: &stacks, steps: steps))
}

func move(stacks: inout [[Character]], steps: [String]) -> String {
    for step in steps {
        let components = step.split(separator: " ")
        let n = Int(components[1])!
        let from = Int(components[3])! - 1
        let to = Int(components[5])! - 1
        
        let moving = stacks[from].suffix(n)
        stacks[from].removeLast(n)
        stacks[to].append(contentsOf: moving)
    }

    return String(stacks.map { $0.last! })
}

solve()

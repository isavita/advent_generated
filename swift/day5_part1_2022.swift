
import Foundation

let input = try String(contentsOfFile: "input.txt")
let s = input.components(separatedBy: "\n\n")
let inputLines = s[0].components(separatedBy: "\n")
var stacks = [[Character]](repeating: [Character](), count: (inputLines[0].count + 1) / 4)

for line in inputLines {
    for (i, b) in line.enumerated() {
        if b >= "A" && b <= "Z" {
            stacks[(i - 1) / 4].append(b)
        }
    }
}

let steps = s[1].components(separatedBy: "\n")
print(move(st: stacks, steps: steps))

func move(st: [[Character]], steps: [String]) -> String {
    var stacks = [[Character]](repeating: [Character](), count: st.count)

    for i in 0..<st.count {
        stacks[i] = Array(st[i].reversed())
    }

    for step in steps {
        let components = step.components(separatedBy: " ")
        let n = Int(components[1])!
        let from = Int(components[3])! - 1
        let to = Int(components[5])! - 1

        for _ in 0..<n {
            stacks[to].append(stacks[from].removeLast())
        }
    }

    var result = ""
    for stack in stacks {
        result.append(stack.last!)
    }

    return result
}

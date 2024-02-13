
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL, encoding: .utf8)
var pipes = [Int: [Int]]()

input.components(separatedBy: .newlines).forEach {
    let parts = $0.components(separatedBy: " <-> ")
    let program = Int(parts[0])!
    let connectedPrograms = parts[1].components(separatedBy: ", ").map { Int($0)! }
    pipes[program] = connectedPrograms
}

func findGroup(for program: Int, in pipes: [Int: [Int]]) -> Set<Int> {
    var group = Set<Int>()
    var queue = [program]

    while !queue.isEmpty {
        let current = queue.removeFirst()
        group.insert(current)

        for connectedProgram in pipes[current] ?? [] {
            if !group.contains(connectedProgram) {
                queue.append(connectedProgram)
            }
        }
    }

    return group
}

let groupContainingZero = findGroup(for: 0, in: pipes)
print(groupContainingZero.count)

var groups = Set<Set<Int>>()

for program in pipes.keys {
    var found = false
    for group in groups {
        if group.contains(program) {
            found = true
            break
        }
    }

    if !found {
        let newGroup = findGroup(for: program, in: pipes)
        groups.insert(newGroup)
    }
}

print(groups.count)

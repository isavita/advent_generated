
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL).trimmingCharacters(in: .whitespacesAndNewlines)

var pipes = [Int: [Int]]()

input.components(separatedBy: .newlines).forEach { line in
    let parts = line.components(separatedBy: " <-> ")
    let program = Int(parts[0])!
    let connectedPrograms = parts[1].components(separatedBy: ", ").map { Int($0)! }
    pipes[program] = connectedPrograms
}

func findGroup(program: Int, visited: inout Set<Int>) {
    visited.insert(program)
    for connectedProgram in pipes[program] ?? [] {
        if !visited.contains(connectedProgram) {
            findGroup(program: connectedProgram, visited: &visited)
        }
    }
}

var groupContainingZero = Set<Int>()
findGroup(program: 0, visited: &groupContainingZero)

print(groupContainingZero.count)

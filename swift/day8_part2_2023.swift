
import Foundation

struct Network {
    var instructions: String
    var nodes: [String: [String]]
}

func parseInput(input: [String]) -> Network {
    let instructions = input[0]
    
    var nodes: [String: [String]] = [:]
    for line in input[2...] {
        let (head, children) = parseLine(line: line)
        nodes[head] = children
    }
    
    return Network(instructions: instructions, nodes: nodes)
}

func parseLine(line: String) -> (String, [String]) {
    let parts = line.components(separatedBy: " = ")
    
    let head = parts[0]
    let childrenTrim = parts[1].trimmingCharacters(in: ["(", ")"])
    let childrenParts = childrenTrim.components(separatedBy: ", ")
    let children = [childrenParts[0], childrenParts[1]]
    
    return (head, children)
}

func gcd(a: Int, b: Int) -> Int {
    var a = a
    var b = b
    while b != 0 {
        (a, b) = (b, a % b)
    }
    return a
}

func lcm(a: Int, b: Int) -> Int {
    return (a * b) / gcd(a: a, b: b)
}

func lcmSlice(nums: [Int]) -> Int {
    if nums.isEmpty {
        return 0
    }
    
    var res = nums[0]
    for i in 1..<nums.count {
        res = lcm(a: res, b: nums[i])
    }
    
    return res
}

func solve(input: [String]) -> Int {
    let network = parseInput(input: input)
    
    var starts: [String] = []
    for node in network.nodes.keys {
        let lastIndex = node.index(before: node.endIndex)
        if node[lastIndex] == "A" {
            starts.append(node)
        }
    }
    
    var steps: [Int] = Array(repeating: 0, count: starts.count)
    let instructionsLength = network.instructions.count
    for i in 0..<starts.count {
        var element = starts[i]
        let lastIndex = element.index(before: element.endIndex)
        while element[lastIndex] != "Z" {
            let instructionIndex = network.instructions.index(network.instructions.startIndex, offsetBy: steps[i] % instructionsLength)
            let instruction = network.instructions[instructionIndex]
            if instruction == "L" {
                element = network.nodes[element]![0]
            } else {
                element = network.nodes[element]![1]
            }
            steps[i] += 1
        }
    }
    
    let res = lcmSlice(nums: steps)
    return res
}

func readFile(fileName: String) -> [String] {
    do {
        let fileContents = try String(contentsOfFile: fileName)
        return fileContents.components(separatedBy: "\n")
    } catch {
        fatalError("Error reading file: \(error)")
    }
}

let input = readFile(fileName: "input.txt")
print(solve(input: input))

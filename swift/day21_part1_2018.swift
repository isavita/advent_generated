
import Foundation

struct Instruction {
    let name: String
    let abcValues: [Int]
}

class OpcodeComputer {
    var registers: [Int]
    let instructions: [Instruction]
    let instructionPointer: Int

    init(instructions: [Instruction], instructionPointer: Int) {
        self.instructions = instructions
        self.registers = Array(repeating: 0, count: 6)
        self.instructionPointer = instructionPointer
    }

    func tick() -> Bool {
        guard registers[instructionPointer] >= 0 && registers[instructionPointer] < instructions.count else {
            return true
        }

        let instIndex = registers[instructionPointer]
        let inst = instructions[instIndex]

        registers = opcodeNamesToFuncs[inst.name]!(registers, inst.abcValues)
        registers[instructionPointer] += 1

        return registers[instructionPointer] >= instructions.count
    }
}

func addr(_ registers: [Int], _ abcValues: [Int]) -> [Int] {
    var newRegisters = registers
    newRegisters[abcValues[2]] = registers[abcValues[0]] + registers[abcValues[1]]
    return newRegisters
}

func addi(_ registers: [Int], _ abcValues: [Int]) -> [Int] {
    var newRegisters = registers
    newRegisters[abcValues[2]] = registers[abcValues[0]] + abcValues[1]
    return newRegisters
}

func mulr(_ registers: [Int], _ abcValues: [Int]) -> [Int] {
    var newRegisters = registers
    newRegisters[abcValues[2]] = registers[abcValues[0]] * registers[abcValues[1]]
    return newRegisters
}

func muli(_ registers: [Int], _ abcValues: [Int]) -> [Int] {
    var newRegisters = registers
    newRegisters[abcValues[2]] = registers[abcValues[0]] * abcValues[1]
    return newRegisters
}

func banr(_ registers: [Int], _ abcValues: [Int]) -> [Int] {
    var newRegisters = registers
    newRegisters[abcValues[2]] = registers[abcValues[0]] & registers[abcValues[1]]
    return newRegisters
}

func bani(_ registers: [Int], _ abcValues: [Int]) -> [Int] {
    var newRegisters = registers
    newRegisters[abcValues[2]] = registers[abcValues[0]] & abcValues[1]
    return newRegisters
}

func borr(_ registers: [Int], _ abcValues: [Int]) -> [Int] {
    var newRegisters = registers
    newRegisters[abcValues[2]] = registers[abcValues[0]] | registers[abcValues[1]]
    return newRegisters
}

func bori(_ registers: [Int], _ abcValues: [Int]) -> [Int] {
    var newRegisters = registers
    newRegisters[abcValues[2]] = registers[abcValues[0]] | abcValues[1]
    return newRegisters
}

func setr(_ registers: [Int], _ abcValues: [Int]) -> [Int] {
    var newRegisters = registers
    newRegisters[abcValues[2]] = registers[abcValues[0]]
    return newRegisters
}

func seti(_ registers: [Int], _ abcValues: [Int]) -> [Int] {
    var newRegisters = registers
    newRegisters[abcValues[2]] = abcValues[0]
    return newRegisters
}

func gtir(_ registers: [Int], _ abcValues: [Int]) -> [Int] {
    var newRegisters = registers
    newRegisters[abcValues[2]] = abcValues[0] > registers[abcValues[1]] ? 1 : 0
    return newRegisters
}

func gtri(_ registers: [Int], _ abcValues: [Int]) -> [Int] {
    var newRegisters = registers
    newRegisters[abcValues[2]] = registers[abcValues[0]] > abcValues[1] ? 1 : 0
    return newRegisters
}

func gtrr(_ registers: [Int], _ abcValues: [Int]) -> [Int] {
    var newRegisters = registers
    newRegisters[abcValues[2]] = registers[abcValues[0]] > registers[abcValues[1]] ? 1 : 0
    return newRegisters
}

func eqir(_ registers: [Int], _ abcValues: [Int]) -> [Int] {
    var newRegisters = registers
    newRegisters[abcValues[2]] = abcValues[0] == registers[abcValues[1]] ? 1 : 0
    return newRegisters
}

func eqri(_ registers: [Int], _ abcValues: [Int]) -> [Int] {
    var newRegisters = registers
    newRegisters[abcValues[2]] = registers[abcValues[0]] == abcValues[1] ? 1 : 0
    return newRegisters
}

func eqrr(_ registers: [Int], _ abcValues: [Int]) -> [Int] {
    var newRegisters = registers
    newRegisters[abcValues[2]] = registers[abcValues[0]] == registers[abcValues[1]] ? 1 : 0
    return newRegisters
}

typealias OpcodeFunc = ([Int], [Int]) -> [Int]

let opcodeNamesToFuncs: [String: OpcodeFunc] = [
    "addr": addr, "addi": addi,
    "mulr": mulr, "muli": muli,
    "banr": banr, "bani": bani,
    "borr": borr, "bori": bori,
    "setr": setr, "seti": seti,
    "gtir": gtir, "gtri": gtri, "gtrr": gtrr,
    "eqir": eqir, "eqri": eqri, "eqrr": eqrr,
]

func parseInput(_ inputData: String) -> OpcodeComputer {
    let lines = inputData.components(separatedBy: "\n")
    let instructionPointer = Int(lines[0].components(separatedBy: " ").last!)!

    var instructions: [Instruction] = []
    for line in lines.dropFirst() {
        guard !line.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty else { continue }
        let parts = line.components(separatedBy: " ")
        let name = parts[0]
        let abcValues = parts.dropFirst().map { Int($0)! }
        instructions.append(Instruction(name: name, abcValues: abcValues))
    }

    return OpcodeComputer(instructions: instructions, instructionPointer: instructionPointer)
}

func solve(_ inputData: String) -> Int {
    let opcodeComputer = parseInput(inputData)

    while !opcodeComputer.tick() {
        if opcodeComputer.registers[opcodeComputer.instructionPointer] == 28 {
            break
        }
    }

    return opcodeComputer.registers[5]
}

func main() {
    do {
        let inputData = try String(contentsOfFile: "input.txt")
        print(solve(inputData))
    } catch {
        print("Error reading input.txt: \(error)")
    }
}

main()

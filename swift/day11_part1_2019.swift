
import Foundation

enum PanelColor: Int {
    case black = 0, white = 1
}

enum Direction: Int {
    case up, right, down, left
}

struct Position: Hashable {
    var x, y: Int
}

class Robot {
    var position: Position
    var direction: Direction
    
    init() {
        self.position = Position(x: 0, y: 0)
        self.direction = .up
    }
    
    func turnAndMove(turnDirection: Int) {
        direction = turnDirection == 0 ? Direction(rawValue: (direction.rawValue + 3) % 4)! : Direction(rawValue: (direction.rawValue + 1) % 4)!
        
        switch direction {
        case .up: position.y -= 1
        case .right: position.x += 1
        case .down: position.y += 1
        case .left: position.x -= 1
        }
    }
}

class Intcode {
    var memory: [Int]
    var ip = 0
    var input: [Int] = []
    var output: [Int] = []
    var halted = false
    
    init(program: [Int]) {
        self.memory = program
    }
    
    func addInput(_ input: Int) {
        self.input.append(input)
    }
    
    func run() {
        output.removeAll()
        while !halted {
            let opcode = memory[ip] % 100
            switch opcode {
            case 1, 2, 7, 8:
                let params = getParams(count: 3)
                let val1 = readMemory(params[0]), val2 = readMemory(params[1])
                if opcode == 1 { writeMemory(params[2], val1 + val2) }
                else if opcode == 2 { writeMemory(params[2], val1 * val2) }
                else if (opcode == 7 && val1 < val2) || (opcode == 8 && val1 == val2) { writeMemory(params[2], 1) }
                else { writeMemory(params[2], 0) }
                ip += 4
            case 3, 4:
                let params = getParams(count: 1)
                if opcode == 3 {
                    guard !input.isEmpty else { return }
                    writeMemory(params[0], input.removeFirst())
                } else {
                    output.append(readMemory(params[0]))
                }
                ip += 2
            case 5, 6:
                let params = getParams(count: 2)
                let val = readMemory(params[0]), target = readMemory(params[1])
                if (opcode == 5 && val != 0) || (opcode == 6 && val == 0) { ip = target }
                else { ip += 3 }
            case 99: halted = true
            default: fatalError("unknown opcode: \(opcode)")
            }
        }
    }
    
    func readMemory(_ address: Int) -> Int {
        ensureMemory(address)
        return memory[address]
    }
    
    func writeMemory(_ address: Int, _ value: Int) {
        ensureMemory(address)
        memory[address] = value
    }
    
    func ensureMemory(_ address: Int) {
        if address >= memory.count {
            memory += Array(repeating: 0, count: address - memory.count + 1)
        }
    }
    
    func getParams(count: Int) -> [Int] {
        var paramModes = memory[ip] / 100
        return (0..<count).map { i in
            let mode = paramModes % 10
            paramModes /= 10
            return mode == 1 ? ip + i + 1 : memory[ip + i + 1]
        }
    }
}

func main() {
    let data = try! String(contentsOfFile: "input.txt")
    let program = data.split(separator: ",").compactMap { Int($0) }
    
    var grid = [Position: PanelColor]()
    let robot = Robot()
    let intcode = Intcode(program: program)
    
    while !intcode.halted {
        let currentColor = grid[robot.position] ?? .black
        intcode.addInput(currentColor.rawValue)
        intcode.run()
        
        if intcode.output.count == 2 {
            grid[robot.position] = PanelColor(rawValue: intcode.output[0])
            robot.turnAndMove(turnDirection: intcode.output[1])
        }
    }
    
    print(grid.count)
}

main()

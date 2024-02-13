
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)
var program = input.components(separatedBy: ",").compactMap { Int($0) }

var output = 0
var i = 0

func getValue(program: [Int], pos: Int, mode: Int) -> Int {
    if mode == 0 {
        return program[program[pos]]
    } else {
        return program[pos]
    }
}

while true {
    let opcode = program[i] % 100
    var modes = program[i] / 100
    let param1Mode = modes % 10
    modes /= 10
    let param2Mode = modes % 10

    switch opcode {
    case 1:
        let p1 = getValue(program: program, pos: i+1, mode: param1Mode)
        let p2 = getValue(program: program, pos: i+2, mode: param2Mode)
        let p3 = program[i+3]
        program[p3] = p1 + p2
        i += 4
    case 2:
        let p1 = getValue(program: program, pos: i+1, mode: param1Mode)
        let p2 = getValue(program: program, pos: i+2, mode: param2Mode)
        let p3 = program[i+3]
        program[p3] = p1 * p2
        i += 4
    case 3:
        program[program[i+1]] = 5
        i += 2
    case 4:
        output = getValue(program: program, pos: i+1, mode: param1Mode)
        print(output)
        i += 2
    case 5:
        let p1 = getValue(program: program, pos: i+1, mode: param1Mode)
        let p2 = getValue(program: program, pos: i+2, mode: param2Mode)
        if p1 != 0 {
            i = p2
        } else {
            i += 3
        }
    case 6:
        let p1 = getValue(program: program, pos: i+1, mode: param1Mode)
        let p2 = getValue(program: program, pos: i+2, mode: param2Mode)
        if p1 == 0 {
            i = p2
        } else {
            i += 3
        }
    case 7:
        let p1 = getValue(program: program, pos: i+1, mode: param1Mode)
        let p2 = getValue(program: program, pos: i+2, mode: param2Mode)
        let p3 = program[i+3]
        if p1 < p2 {
            program[p3] = 1
        } else {
            program[p3] = 0
        }
        i += 4
    case 8:
        let p1 = getValue(program: program, pos: i+1, mode: param1Mode)
        let p2 = getValue(program: program, pos: i+2, mode: param2Mode)
        let p3 = program[i+3]
        if p1 == p2 {
            program[p3] = 1
        } else {
            program[p3] = 0
        }
        i += 4
    case 99:
        exit(0)
    default:
        fatalError("Invalid opcode")
    }
}

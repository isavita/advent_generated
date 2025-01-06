
import Foundation

func scramble(_ password: inout [Character], _ instructions: [String]) {
    for instruction in instructions {
        let parts = instruction.split(separator: " ")
        switch parts[0] {
        case "swap":
            if parts[1] == "position" {
                let x = Int(parts[2])!
                let y = Int(parts[5])!
                password.swapAt(x, y)
            } else {
                let x = parts[2].first!
                let y = parts[5].first!
                let xIndex = password.firstIndex(of: x)!
                let yIndex = password.firstIndex(of: y)!
                password.swapAt(xIndex, yIndex)
            }
        case "rotate":
            if parts[1] == "based" {
                let x = parts[6].first!
                let index = password.firstIndex(of: x)!
                var rotations = 1 + index
                if index >= 4 {
                    rotations += 1
                }
                for _ in 0..<rotations {
                    password.insert(password.removeLast(), at: 0)
                }
            } else {
                let steps = Int(parts[2])!
                if parts[1] == "left" {
                    for _ in 0..<steps {
                        password.append(password.removeFirst())
                    }
                } else {
                    for _ in 0..<steps {
                        password.insert(password.removeLast(), at: 0)
                    }
                }
            }
        case "reverse":
            let x = Int(parts[2])!
            let y = Int(parts[4])!
            password[x...y].reverse()
        case "move":
            let x = Int(parts[2])!
            let y = Int(parts[5])!
            let char = password.remove(at: x)
            password.insert(char, at: y)
        default:
            break
        }
    }
}

func unscramble(_ password: inout [Character], _ instructions: [String]) {
    for instruction in instructions.reversed() {
        let parts = instruction.split(separator: " ")
        switch parts[0] {
        case "swap":
            if parts[1] == "position" {
                let x = Int(parts[2])!
                let y = Int(parts[5])!
                password.swapAt(x, y)
            } else {
                let x = parts[2].first!
                let y = parts[5].first!
                let xIndex = password.firstIndex(of: x)!
                let yIndex = password.firstIndex(of: y)!
                password.swapAt(xIndex, yIndex)
            }
        case "rotate":
            if parts[1] == "based" {
                let x = parts[6].first!
                var originalIndex = -1
                for i in 0..<password.count {
                    var tempPassword = password
                    for _ in 0..<i {
                        tempPassword.insert(tempPassword.removeLast(), at: 0)
                    }
                    
                    let index = tempPassword.firstIndex(of: x)!
                    var rotations = 1 + index
                    if index >= 4 {
                        rotations += 1
                    }
                    for _ in 0..<rotations {
                        tempPassword.insert(tempPassword.removeLast(), at: 0)
                    }
                    if tempPassword == password {
                        originalIndex = i
                        break
                    }
                }
                
                for _ in 0..<originalIndex {
                    password.insert(password.removeLast(), at: 0)
                }
            } else {
                let steps = Int(parts[2])!
                if parts[1] == "left" {
                    for _ in 0..<steps {
                        password.insert(password.removeLast(), at: 0)
                    }
                } else {
                    for _ in 0..<steps {
                        password.append(password.removeFirst())
                    }
                }
            }
        case "reverse":
            let x = Int(parts[2])!
            let y = Int(parts[4])!
            password[x...y].reverse()
        case "move":
            let x = Int(parts[2])!
            let y = Int(parts[5])!
            let char = password.remove(at: y)
            password.insert(char, at: x)
        default:
            break
        }
    }
}

if let input = try? String(contentsOfFile: "input.txt") {
    let instructions = input.trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: "\n")

    var passwordPart1 = Array("abcdefgh")
    scramble(&passwordPart1, instructions)
    print(String(passwordPart1))

    var passwordPart2 = Array("fbgdceah")
    unscramble(&passwordPart2, instructions)
    print(String(passwordPart2))
}

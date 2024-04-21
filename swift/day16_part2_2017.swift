import Foundation

func spin(_ programs: inout [Character], _ x: Int) {
    let n = programs.count
    let temp = programs
    for i in 0..<n {
        programs[(i+x)%n] = temp[i]
    }
}

func exchange(_ programs: inout [Character], _ A: Int, _ B: Int) {
    programs.swapAt(A, B)
}

func partner(_ programs: inout [Character], _ A: Character, _ B: Character) {
    if let indexA = programs.firstIndex(of: A), let indexB = programs.firstIndex(of: B) {
        exchange(&programs, indexA, indexB)
    }
}

func main() {
    do {
        let fileContent = try String(contentsOfFile: "input.txt", encoding: .utf8)
        let moves = fileContent.components(separatedBy: ",")

        var programs: [Character] = Array("abcdefghijklmnop")
        let initial = String(programs)
        var cycleLen = 0

        for iteration in 0..<1000000000 {
            for move in moves {
                switch move.first {
                case "s":
                    let x = Int(move.dropFirst().description)!
                    spin(&programs, x)
                case "x":
                    let positions = move.dropFirst().components(separatedBy: "/")
                    let A = Int(positions[0])!
                    let B = Int(positions[1])!
                    exchange(&programs, A, B)
                case "p":
                    let positions = move.dropFirst().components(separatedBy: "/")
                    let A = Character(positions[0])
                    let B = Character(positions[1])
                    partner(&programs, A, B)
                default:
                    break
                }
            }

            if String(programs) == initial {
                cycleLen = iteration + 1
                break
            }
        }

        programs = Array(initial)

        for _ in 0..<1000000000 % cycleLen {
            for move in moves {
                switch move.first {
                case "s":
                    let x = Int(move.dropFirst().description)!
                    spin(&programs, x)
                case "x":
                    let positions = move.dropFirst().components(separatedBy: "/")
                    let A = Int(positions[0])!
                    let B = Int(positions[1])!
                    exchange(&programs, A, B)
                case "p":
                    let positions = move.dropFirst().components(separatedBy: "/")
                    let A = Character(positions[0])
                    let B = Character(positions[1])
                    partner(&programs, A, B)
                default:
                    break
                }
            }
        }

        print(String(programs))
    } catch {
        print("Error reading file:", error)
    }
}

main()
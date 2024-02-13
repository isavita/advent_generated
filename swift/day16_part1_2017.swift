
import Foundation

var programs = Array("abcdefghijklmnop")
if let input = try? String(contentsOfFile: "input.txt") {
    let moves = input.components(separatedBy: ",")

    for move in moves {
        switch move.first {
        case "s":
            let x = Int(move.dropFirst()) ?? 0
            spin(&programs, x)
        case "x":
            let positions = move.dropFirst().components(separatedBy: "/")
            let A = Int(positions[0]) ?? 0
            let B = Int(positions[1]) ?? 0
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

    print(String(programs))
}

func spin(_ programs: inout [Character], _ x: Int) {
    let n = programs.count
    var temp = programs

    for i in 0..<n {
        programs[(i+x)%n] = temp[i]
    }
}

func exchange(_ programs: inout [Character], _ A: Int, _ B: Int) {
    programs.swapAt(A, B)
}

func partner(_ programs: inout [Character], _ A: Character, _ B: Character) {
    guard let indexA = programs.firstIndex(of: A), let indexB = programs.firstIndex(of: B) else { return }
    programs.swapAt(indexA, indexB)
}

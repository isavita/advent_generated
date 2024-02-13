
import Foundation

func readInput() -> String {
    let fileURL = URL(fileURLWithPath: "input.txt")
    return try! String(contentsOf: fileURL)
}

func num(_ w: [Int]) -> Int {
    var n = 0
    for i in w {
        n *= 10
        n += i
    }
    return n
}

func solution() {
    let input = readInput()
    var k = [Int]()
    var l = [Int]()
    var m = [Int]()
    
    let lines = input.components(separatedBy: "\n")
    for (i, line) in lines.enumerated() {
        var v = 0
        switch i % 18 {
        case 4:
            v = Int(line.components(separatedBy: " ")[2])!
            l.append(v)
        case 5:
            v = Int(line.components(separatedBy: " ")[2])!
            k.append(v)
        case 15:
            v = Int(line.components(separatedBy: " ")[2])!
            m.append(v)
        default:
            break
        }
    }
    
    var constraints = [Int: (Int, Int)]()
    var stack = [Int]()
    for i in 0..<l.count {
        switch l[i] {
        case 1:
            stack.append(i)
        case 26:
            let pop = stack.removeLast()
            constraints[pop] = (i, m[pop] + k[i])
        default:
            break
        }
    }
    
    var max = [Int](repeating: 0, count: 14)
    for i in 0..<14 {
        if constraints[i] == nil {
            continue
        }
        var vmax = 9
        while vmax + constraints[i]!.1 > 9 {
            vmax -= 1
        }
        max[i] = vmax
        max[constraints[i]!.0] = vmax + constraints[i]!.1
    }
    
    print(num(max))
}

solution()

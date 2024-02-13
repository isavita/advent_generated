
import Foundation

func readInput() -> String {
    let fileURL = URL(fileURLWithPath: "input.txt")
    return try! String(contentsOf: fileURL)
}

func main() {
    let input = readInput()
    var k = [Int]()
    var l = [Int]()
    var m = [Int]()
    let lines = input.components(separatedBy: "\n")
    
    for i in 0..<lines.count {
        let line = lines[i]
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
    
    var min = [Int](repeating: 0, count: 14)
    
    for i in 0..<14 {
        if constraints[i] == nil {
            continue
        }
        var vmin = 1
        while vmin + constraints[i]!.1 < 1 {
            vmin += 1
        }
        min[i] = vmin
        min[constraints[i]!.0] = vmin + constraints[i]!.1
    }
    
    print(num(w: min))
}

func num(w: [Int]) -> Int {
    var n = 0
    for i in 0..<w.count {
        n *= 10
        n += w[i]
    }
    return n
}

main()

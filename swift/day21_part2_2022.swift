
import Foundation

class Monkey {
    var name: String
    var val: Int?
    var left: Monkey?
    var right: Monkey?
    var op: String?

    init(name: String) {
        self.name = name
    }

    func solve() -> (Int, Bool) {
        if let value = val {
            return (value, true)
        }

        guard let left = left, let right = right else { return (0, false) }
        let (leftVal, lOk) = left.solve()
        let (rightVal, rOk) = right.solve()

        if lOk && rOk {
            switch op {
            case "+": return (leftVal + rightVal, true)
            case "-": return (leftVal - rightVal, true)
            case "*": return (leftVal * rightVal, true)
            case "/": return (leftVal / rightVal, true)
            case "==": return (leftVal == rightVal ? 0 : 1, true)
            default: return (0, false)
            }
        }
        return (0, false)
    }

    func expect(_ x: Int) -> Int {
        if name == "humn" {
            return x
        }

        let (leftVal, lOk) = left?.solve() ?? (0, false)
        let (rightVal, rOk) = right?.solve() ?? (0, false)

        if !lOk {
            switch op {
            case "+": return left!.expect(x - rightVal)
            case "-": return left!.expect(x + rightVal)
            case "*": return left!.expect(x / rightVal)
            case "/": return left!.expect(x * rightVal)
            case "==": return left!.expect(rightVal)
            default: break
            }
        }

        if !rOk {
            switch op {
            case "+": return right!.expect(x - leftVal)
            case "-": return right!.expect(leftVal - x)
            case "*": return right!.expect(x / leftVal)
            case "/": return right!.expect(leftVal / x)
            case "==": return right!.expect(leftVal)
            default: break
            }
        }

        fatalError("impossible")
    }
}

func parse() -> [String: Monkey] {
    var index = [String: Monkey]()

    func initMonkey(_ name: String) {
        if index[name] == nil {
            index[name] = Monkey(name: name)
        }
    }

    let fileURL = URL(fileURLWithPath: "input.txt")
    let content = try! String(contentsOf: fileURL)
    let lines = content.split(separator: "\n")

    for line in lines {
        let parts = line.split(separator: ": ")
        let goal = String(parts[0])
        initMonkey(goal)

        if let num = Int(parts[1]) {
            index[goal]?.val = num
            continue
        }

        let r = parts[1].split(separator: " ")
        let left = String(r[0]), op = String(r[1]), right = String(r[2])
        initMonkey(left)
        initMonkey(right)

        index[goal]?.left = index[left]
        index[goal]?.op = op
        index[goal]?.right = index[right]
    }
    return index
}

let index = parse()
index["humn"]?.val = nil
index["root"]?.op = "=="
print(index["root"]!.expect(0))

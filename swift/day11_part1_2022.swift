
import Foundation

struct Monkey {
    var items: [Int]
    var operation: (Int) -> Int
    var div: Int
    var next: (Int, Int)
    var inspections: Int = 0
}

func parse(_ s: String) -> Monkey {
    let lines = s.split(separator: "\n").map(String.init)
    var items = lines[1].split(separator: ":")[1].split(separator: ",").map { Int($0.trimmingCharacters(in: .whitespaces))! }
    
    let operationLine = lines[2].split(separator: "=")[1].split(separator: " ")
    let operation: (Int) -> Int
    if operationLine[1] == "+" {
        if operationLine[2] == "old" {
            operation = { $0 + $0 }
        } else {
            let value = Int(operationLine[2])!
            operation = { $0 + value }
        }
    } else {
        if operationLine[2] == "old" {
            operation = { $0 * $0 }
        } else {
            let value = Int(operationLine[2])!
            operation = { $0 * value }
        }
    }
    
    let div = Int(lines[3].split(separator: " ").last!)!
    let nextTrue = Int(lines[4].split(separator: " ").last!)!
    let nextFalse = Int(lines[5].split(separator: " ").last!)!
    
    return Monkey(items: items, operation: operation, div: div, next: (nextTrue, nextFalse))
}

func monkeyBusiness(monkeys: inout [Monkey], rounds: Int, worry: Bool) -> Int {
    let div = monkeys.reduce(1) { $0 * $1.div }
    
    for _ in 0..<rounds {
        for i in 0..<monkeys.count {
            while !monkeys[i].items.isEmpty {
                monkeys[i].inspections += 1
                var item = monkeys[i].operation(monkeys[i].items.removeFirst())
                item = worry ? item % div : item / 3
                
                let nextMonkey = item % monkeys[i].div == 0 ? monkeys[i].next.0 : monkeys[i].next.1
                monkeys[nextMonkey].items.append(item)
            }
        }
    }
    
    let inspections = monkeys.map { $0.inspections }.sorted(by: >)
    return inspections[0] * inspections[1]
}

func readAll(from path: String) -> String {
    return try! String(contentsOfFile: path)
}

let input = readAll(from: "input.txt")
var monkeys = input.split(separator: "\n\n").map { parse(String($0)) }
print(monkeyBusiness(monkeys: &monkeys, rounds: 20, worry: false))

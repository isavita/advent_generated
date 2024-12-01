
import Foundation

let hashTableSize = 256

struct Step {
    var label: String
    var numBox: Int
    var operation: String
    var number: Int
}

func hashString(_ str: String) -> Int {
    var res = 0
    for char in str {
        res += Int(char.asciiValue!)
        res *= 17
        res %= hashTableSize
    }
    return res
}

func parseStep(_ stepStr: String) -> Step {
    let parts = stepStr.split(separator: "=", maxSplits: 1)
    let label = String(parts[0]).trimmingCharacters(in: .alphanumerics.inverted)
    let numBox = hashString(label)
    let operation = parts.count > 1 ? "=" : "-"
    let number = operation == "=" ? Int(parts[1])! : 0
    return Step(label: label, numBox: numBox, operation: operation, number: number)
}

func getBoxes(_ stepsStr: [String]) -> [Int:[[String:Int]]] {
    var boxes: [Int:[[String:Int]]] = [:]
    for stepStr in stepsStr {
        let step = parseStep(stepStr)
        if var boxContents = boxes[step.numBox] {
            switch step.operation {
            case "-":
                boxContents.removeAll(where: { $0.keys.contains(step.label) })
            case "=":
                if let index = boxContents.firstIndex(where: { $0.keys.contains(step.label) }) {
                    boxContents[index][step.label] = step.number
                } else {
                    boxContents.append([step.label: step.number])
                }
            default:
                break
            }
            boxes[step.numBox] = boxContents.filter{!$0.isEmpty}
        } else {
            if step.operation == "=" {
                boxes[step.numBox] = [[step.label: step.number]]
            }
        }
    }
    return boxes
}


func calculatePower(_ boxes: [Int:[[String:Int]]]) -> Int {
    var res = 0
    for (iBox, boxContents) in boxes {
        for (iSlot, content) in boxContents.enumerated() {
            for (_, value) in content {
                res += (iBox + 1) * (iSlot + 1) * value
            }
        }
    }
    return res
}

func solve(_ input: String) -> Int {
    let stepsStr = input.split(separator: ",").map{String($0)}
    let boxes = getBoxes(stepsStr)
    return calculatePower(boxes)
}

func main() {
    do {
        let fileURL = URL(fileURLWithPath: "input.txt")
        let inputString = try String(contentsOf: fileURL)
        let result = solve(inputString)
        print(result)
    } catch {
        print("Error reading file: \(error)")
    }
}

main()


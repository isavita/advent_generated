
import Foundation

func someAssemblyRequired(input: String) -> Int {
    var wireToRule = [String: String]()
    for inst in input.split(separator: "\n") {
        let parts = inst.split(separator: " -> ")
        wireToRule[String(parts[1])] = String(parts[0])
    }

    var memo = [String: Int]()
    func memoDFS(entry: String) -> Int {
        if let memoVal = memo[entry] {
            return memoVal
        }
        if let num = Int(entry) {
            return num
        }

        let sourceRule = wireToRule[entry]!
        let parts = sourceRule.split(separator: " ")
        let result: Int

        if parts.count == 1 {
            result = memoDFS(entry: String(parts[0]))
        } else if parts[0] == "NOT" {
            result = 65535 ^ memoDFS(entry: String(parts[1]))
        } else if parts[1] == "AND" {
            result = memoDFS(entry: String(parts[0])) & memoDFS(entry: String(parts[2]))
        } else if parts[1] == "OR" {
            result = memoDFS(entry: String(parts[0])) | memoDFS(entry: String(parts[2]))
        } else if parts[1] == "LSHIFT" {
            result = memoDFS(entry: String(parts[0])) << memoDFS(entry: String(parts[2]))
        } else {
            result = memoDFS(entry: String(parts[0])) >> memoDFS(entry: String(parts[2]))
        }

        memo[entry] = result
        return result
    }

    let aSignal = memoDFS(entry: "a")
    wireToRule["b"] = String(aSignal)
    memo = [:]
    return memoDFS(entry: "a")
}

let input = try! String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines)
print(someAssemblyRequired(input: input))

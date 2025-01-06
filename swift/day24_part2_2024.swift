
import Foundation

struct Gate {
    let a: String
    let op: String
    let b: String
}

func parse(input: String) -> [(gate: Gate, output: String)]? {
    let parts = input.components(separatedBy: "\n\n")
    guard parts.count == 2 else { return nil }

    var gates: [(gate: Gate, output: String)] = []
    for line in parts[1].components(separatedBy: "\n") {
        guard !line.isEmpty else { continue }
        let parts = line.components(separatedBy: " -> ")
        guard parts.count == 2 else { continue }
        let gateParts = parts[0].components(separatedBy: " ")
        guard gateParts.count == 3 else { continue }
        gates.append((gate: Gate(a: gateParts[0], op: gateParts[1], b: gateParts[2]), output: parts[1]))
    }
    return gates
}

func createLookups(gates: [(gate: Gate, output: String)]) -> (lookup: [String: Gate], reverseLookup: [String: String]) {
    var lookup: [String: Gate] = [:]
    var reverseLookup: [String: String] = [:]

    for g in gates {
        lookup[g.output] = g.gate
        let inputs = [g.gate.a, g.gate.b].sorted()
        let key = "\(inputs[0])_\(g.gate.op)_\(inputs[1])"
        reverseLookup[key] = g.output
    }
    return (lookup, reverseLookup)
}

func swap(pairs: inout [[String]], gates: inout [(gate: Gate, output: String)], a: String, b: String) {
    pairs.append([a, b])
    for i in 0..<gates.count {
        if gates[i].output == a {
            gates[i].output = b
        } else if gates[i].output == b {
            gates[i].output = a
        }
    }
}

func getReverseLookupKey(a: String, op: String, b: String) -> String {
    let inputs = [a, b].sorted()
    return "\(inputs[0])_\(op)_\(inputs[1])"
}

func solution(gates: [(gate: Gate, output: String)]) -> String {
    var pairs: [[String]] = []
    var mutableGates = gates
    let numZ = gates.filter { $0.output.hasPrefix("z") }.count

    while pairs.count < 4 {
        var adder = ""
        var carry = ""
        let (lookup, reverseLookup) = createLookups(gates: mutableGates)

        for i in 0..<numZ {
            let xi = String(format: "x%02d", i)
            let yi = String(format: "y%02d", i)
            let zi = String(format: "z%02d", i)

            if i == 0 {
                adder = reverseLookup[getReverseLookupKey(a: xi, op: "XOR", b: yi)] ?? ""
                carry = reverseLookup[getReverseLookupKey(a: xi, op: "AND", b: yi)] ?? ""
            } else {
                let bit = reverseLookup[getReverseLookupKey(a: xi, op: "XOR", b: yi)] ?? ""
                if !bit.isEmpty {
                    adder = reverseLookup[getReverseLookupKey(a: bit, op: "XOR", b: carry)] ?? ""
                    if !adder.isEmpty {
                        let c1 = reverseLookup[getReverseLookupKey(a: xi, op: "AND", b: yi)] ?? ""
                        let c2 = reverseLookup[getReverseLookupKey(a: bit, op: "AND", b: carry)] ?? ""
                        carry = reverseLookup[getReverseLookupKey(a: c1, op: "OR", b: c2)] ?? ""
                    }
                }
            }

            if adder.isEmpty {
                guard let gate = lookup[zi] else { continue }
                let bitKey = getReverseLookupKey(a: xi, op: "XOR", b: yi)
                let bit = reverseLookup[bitKey] ?? ""
                if reverseLookup[getReverseLookupKey(a: gate.a, op: "XOR", b: carry)] != nil {
                    swap(pairs: &pairs, gates: &mutableGates, a: bit, b: gate.a)
                    break
                } else if reverseLookup[getReverseLookupKey(a: gate.b, op: "XOR", b: carry)] != nil {
                    swap(pairs: &pairs, gates: &mutableGates, a: bit, b: gate.b)
                    break
                }
            } else if adder != zi {
                swap(pairs: &pairs, gates: &mutableGates, a: adder, b: zi)
                break
            }
        }
    }

    let result = pairs.flatMap { $0 }.sorted()
    return result.joined(separator: ",")
}

func main() {
    guard let input = try? String(contentsOfFile: "input.txt") else {
        print("Error reading input file")
        return
    }

    guard let gates = parse(input: input) else {
        print("Error parsing input")
        return
    }

    print(solution(gates: gates))
}

main()


import Foundation

struct IPAddress {
    let supernetSequences: [String]
    let hypernetSequences: [String]

    init(_ address: String) {
        var supernet: [String] = []
        var hypernet: [String] = []
        var current = ""
        var inHypernet = false

        for char in address {
            switch char {
            case "[":
                supernet.append(current)
                current = ""
                inHypernet = true
            case "]":
                hypernet.append(current)
                current = ""
                inHypernet = false
            default:
                current.append(char)
            }
        }
        if !current.isEmpty {
            supernet.append(current)
        }

        self.supernetSequences = supernet
        self.hypernetSequences = hypernet
    }

    func supportsTLS() -> Bool {
        func hasABBA(_ s: String) -> Bool {
            let chars = Array(s)
            for i in 0..<(chars.count - 3) {
                if chars[i] != chars[i+1] && chars[i] == chars[i+3] && chars[i+1] == chars[i+2] {
                    return true
                }
            }
            return false
        }

        for sequence in hypernetSequences {
            if hasABBA(sequence) {
                return false
            }
        }

        for sequence in supernetSequences {
            if hasABBA(sequence) {
                return true
            }
        }

        return false
    }

    func supportsSSL() -> Bool {
        func findABAs(_ s: String) -> Set<String> {
            var abas: Set<String> = []
            let chars = Array(s)
            for i in 0..<(chars.count - 2) {
                if chars[i] != chars[i+1] && chars[i] == chars[i+2] {
                    abas.insert(String(chars[i...i+2]))
                }
            }
            return abas
        }

        var supernetABAs: Set<String> = []
        for sequence in supernetSequences {
            supernetABAs.formUnion(findABAs(sequence))
        }

        for sequence in hypernetSequences {
            let hypernetChars = Array(sequence)
            for i in 0..<(hypernetChars.count - 2) {
                if hypernetChars[i] != hypernetChars[i+1] && hypernetChars[i] == hypernetChars[i+2] {
                    let bab = String([hypernetChars[i+1], hypernetChars[i], hypernetChars[i+1]])
                    if supernetABAs.contains(bab) {
                        return true
                    }
                }
            }
        }

        return false
    }
}


func solve() {
    guard let input = try? String(contentsOfFile: "input.txt") else {
        print("Error reading input file")
        return
    }

    let lines = input.trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: .newlines)
    let ipAddresses = lines.map { IPAddress($0) }

    let tlsCount = ipAddresses.filter { $0.supportsTLS() }.count
    print("Number of IPs supporting TLS: \(tlsCount)")

    let sslCount = ipAddresses.filter { $0.supportsSSL() }.count
    print("Number of IPs supporting SSL: \(sslCount)")
}


// Create a main entry point
solve()

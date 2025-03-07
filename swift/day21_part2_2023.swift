
import Foundation

func parseData(data: [String]) -> (garden: [Complex: Bool], start: Complex) {
    var garden: [Complex: Bool] = [:]
    var start: Complex? = nil

    for y in 0..<data.count {
        let line = data[y]
        for x in 0..<line.count {
            let index = line.index(line.startIndex, offsetBy: x)
            let c = line[index]
            if c != "#" {
                garden[Complex(Double(x), Double(y))] = true
            }
            if c == "S" {
                start = Complex(Double(x), Double(y))
            }
        }
    }

    if start == nil {
        fatalError("No start found!")
    }

    return (garden, start!)
}

func complexMod(num: Complex, mod: Int) -> Complex {
    guard num.real.truncatingRemainder(dividingBy: 1) == 0 && num.imag.truncatingRemainder(dividingBy: 1) == 0 else {
        fatalError("Complex number \(num) is not integer!")
    }
    return Complex(Double((Int(num.real) % mod + mod) % mod), Double((Int(num.imag) % mod + mod) % mod))
}

func calculateNumEnds(garden: [Complex: Bool], start: Complex, numIterations: Int, maxSize: Int) -> Int {
    var queue: [Complex: Bool] = [start: true]
    var done: [Int] = []

    for i in 0..<3 * maxSize {
        if (i % maxSize) == (maxSize - 1) / 2 {
            done.append(queue.count)
        }
        if done.count == 3 {
            break
        }

        var newQueue: [Complex: Bool] = [:]
        for point in queue.keys {
            for dir in [Complex(1, 0), Complex(-1, 0), Complex(0, 1), Complex(0, -1)] {
                let nextPoint = point + dir
                if garden[complexMod(num: nextPoint, mod: maxSize)] != nil {
                    newQueue[nextPoint] = true
                }
            }
        }
        queue = newQueue
    }

    func quadraticFunction(n: Int, a: Int, b: Int, c: Int) -> Int {
        return a + n * (b - a + ((n - 1) * (c - 2 * b + a) / 2))
    }

    return quadraticFunction(n: numIterations / maxSize, a: done[0], b: done[1], c: done[2])
}

struct Complex: Hashable {
    let real: Double
    let imag: Double

    init(_ real: Double, _ imag: Double) {
        self.real = real
        self.imag = imag
    }

    static func +(lhs: Complex, rhs: Complex) -> Complex {
        return Complex(lhs.real + rhs.real, lhs.imag + rhs.imag)
    }
}

do {
    let fileURL = URL(fileURLWithPath: "input.txt")
    let gardenInput = try String(contentsOf: fileURL, encoding: .utf8)
        .components(separatedBy: .newlines)
        .filter { !$0.isEmpty }

    let (garden, start) = parseData(data: gardenInput)
    let maxSize = gardenInput.count
    print(calculateNumEnds(garden: garden, start: start, numIterations: 26501365, maxSize: maxSize))

} catch {
    print("Error reading input.txt: \(error)")
}

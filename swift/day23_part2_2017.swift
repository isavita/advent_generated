
import Foundation

func isPrime(_ n: Int) -> Bool {
    for i in 2...Int(sqrt(Double(n))) {
        if n % i == 0 {
            return false
        }
    }
    return true
}

if let input = try? String(contentsOfFile: "input.txt") {
    let values = input.components(separatedBy: .newlines)
    let b = 57*100 + 100000
    let c = b + 17000
    var h = 0

    for x in stride(from: b, through: c, by: 17) {
        if !isPrime(x) {
            h += 1
        }
    }

    print(h)
}

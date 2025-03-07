
import Foundation

func nextSecret(s: inout Int) {
    let mod = 1 << 24
    var x = s * 64
    s ^= x
    s &= mod - 1
    x = s / 32
    s ^= x
    s &= mod - 1
    x = s * 2048
    s ^= x
    s &= mod - 1
}

func encodeChange4(c1: Int, c2: Int, c3: Int, c4: Int) -> Int {
    return (c1 + 9) + (c2 + 9) * 19 + (c3 + 9) * 19 * 19 + (c4 + 9) * 19 * 19 * 19
}

func solve() {
    let numSteps = 2000
    let patternCount = 19 * 19 * 19 * 19

    guard let input = try? String(contentsOfFile: "input.txt") else {
        return
    }
    let initials = input.split(separator: "\n").compactMap { Int($0) }

    var globalSum = [Int](repeating: 0, count: patternCount)

    for initVal in initials {
        var prices = [Int]()
        var s = initVal
        for _ in 0...numSteps {
            prices.append(s % 10)
            nextSecret(s: &s)
        }

      
        var localPrice = [Int](repeating: -1, count: patternCount)

        for i in 0..<(numSteps - 3) {
            let c1 = prices[i + 1] - prices[i]
            let c2 = prices[i + 2] - prices[i + 1]
            let c3 = prices[i + 3] - prices[i + 2]
            let c4 = prices[i + 4] - prices[i + 3]

            if (-9...9).contains(c1) && (-9...9).contains(c2) && (-9...9).contains(c3) && (-9...9).contains(c4) {
                let idx = encodeChange4(c1: c1, c2: c2, c3: c3, c4: c4)
                if localPrice[idx] < 0 {
                    localPrice[idx] = prices[i + 4]
                }
            }
        }
        
        for (idx, p) in localPrice.enumerated() {
            if p >= 0 {
                globalSum[idx] += p
            }
        }

    }
  
    print(globalSum.max()!)
}

solve()

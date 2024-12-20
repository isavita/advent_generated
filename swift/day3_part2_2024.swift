
import Foundation

func solve() {
    guard let data = try? String(contentsOfFile: "input.txt") else {
        fatalError("Failed to read input file")
    }

    let regex = try! NSRegularExpression(pattern: "(mul\\([0-9]{1,3},[0-9]{1,3}\\))|(do\\(\\))|(don't\\(\\))")
    let matches = regex.matches(in: data, range: NSRange(data.startIndex..., in: data))

    var enabled = true
    var totalSum = 0

    for match in matches {
        if let mulRange = Range(match.range(at: 1), in: data) {
            if enabled {
                let mulStr = String(data[mulRange])
                let numsStr = mulStr.dropFirst(4).dropLast()
                let nums = numsStr.split(separator: ",")
                if nums.count == 2, let x = Int(nums[0]), let y = Int(nums[1]) {
                    totalSum += x * y
                }
            }
        } else if match.range(at: 2).location != NSNotFound {
            enabled = true
        } else if match.range(at: 3).location != NSNotFound {
            enabled = false
        }
    }

    print(totalSum)
}

solve()

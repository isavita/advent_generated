
import Foundation

let input = try String(contentsOfFile: "input.txt")
let vals = input.split(separator: "\n").compactMap { Int($0) }

var count = 0
let n = vals.count

for i in 3..<n {
    let prevSum = vals[i-3] + vals[i-2] + vals[i-1]
    let currSum = vals[i-2] + vals[i-1] + vals[i]
    if currSum > prevSum {
        count += 1
    }
}

print(count)


import Foundation

let input = try String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines)
let lanternfishAges = input.components(separatedBy: ",").compactMap { Int($0) }

var currentAges = lanternfishAges
for day in 1...80 {
    var newAges: [Int] = []
    for age in currentAges {
        if age == 0 {
            newAges.append(6)
            newAges.append(8)
        } else {
            newAges.append(age - 1)
        }
    }
    currentAges = newAges
}

print(currentAges.count)

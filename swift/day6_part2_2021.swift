import Foundation

var lanternFishCounts = [Int](repeating: 0, count: 9)

do {
    let fileContent = try String(contentsOfFile: "input.txt", encoding: .utf8)
    let fishAges = fileContent.components(separatedBy: ",")
    for age in fishAges {
        lanternFishCounts[Int(age)!] += 1
    }
}

for _ in 0...255 {
    let newLanternFish = lanternFishCounts[0]
    for j in 0..<8 {
        lanternFishCounts[j] = lanternFishCounts[j+1]
    }
    lanternFishCounts[6] += newLanternFish
    lanternFishCounts[8] = newLanternFish
}

print(lanternFishCounts.reduce(0, +))
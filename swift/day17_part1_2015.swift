
import Foundation

let input = try String(contentsOfFile: "input.txt")
let containers = input.components(separatedBy: "\n").compactMap { Int($0) }

func combinationsCount(containers: [Int], target: Int, index: Int, current: Int) -> Int {
    if current == target {
        return 1
    }
    if current > target || index == containers.count {
        return 0
    }
    
    return combinationsCount(containers: containers, target: target, index: index + 1, current: current) +
           combinationsCount(containers: containers, target: target, index: index + 1, current: current + containers[index])
}

let result = combinationsCount(containers: containers, target: 150, index: 0, current: 0)
print(result)

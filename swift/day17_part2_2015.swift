
import Foundation

func readInput(from file: String) -> [Int] {
    do {
        let contents = try String(contentsOfFile: file)
        return contents.split(separator: "\n").compactMap { Int($0) }
    } catch {
        print("Error reading file: \(error)")
        return []
    }
}

func findCombinations(containers: [Int], target: Int, start: Int, currentCombination: [Int], allCombinations: inout [[Int]]) {
    if target == 0 {
        allCombinations.append(currentCombination)
        return
    }
    
    for i in start..<containers.count {
        if containers[i] > target {
            continue
        }
        findCombinations(containers: containers, target: target - containers[i], start: i + 1, currentCombination: currentCombination + [containers[i]], allCombinations: &allCombinations)
    }
}

func main() {
    let containers = readInput(from: "input.txt")
    let target = 150
    var allCombinations: [[Int]] = []
    
    findCombinations(containers: containers.sorted(), target: target, start: 0, currentCombination: [], allCombinations: &allCombinations)
    
    // Part 1: Total combinations
    let totalCombinations = allCombinations.count
    print("Total combinations to fit 150 liters: \(totalCombinations)")
    
    // Part 2: Minimum number of containers
    let minContainerCount = allCombinations.map { $0.count }.min() ?? 0
    let waysWithMinContainers = allCombinations.filter { $0.count == minContainerCount }.count
    
    print("Minimum number of containers: \(minContainerCount)")
    print("Ways to fill with minimum containers: \(waysWithMinContainers)")
}

main()


import Foundation

struct RebootStep {
    let action: String
    let xStart: Int
    let xEnd: Int
    let yStart: Int
    let yEnd: Int
    let zStart: Int
    let zEnd: Int
}

func main() {
    let fileURL = URL(fileURLWithPath: "input.txt")
    guard let input = try? String(contentsOf: fileURL) else { return }
    
    let rebootSteps = input.split(separator: "\n").compactMap { parseRebootStep(String($0)) }
    
    let minCoord = -50
    let maxCoord = 50
    var cubeGrid = Array(repeating: Array(repeating: Array(repeating: false, count: maxCoord - minCoord + 1), count: maxCoord - minCoord + 1), count: maxCoord - minCoord + 1)
    
    executeRebootSteps(&cubeGrid, rebootSteps)
    let onCubes = countOnCubes(cubeGrid)
    
    print(onCubes)
}

func parseRebootStep(_ line: String) -> RebootStep? {
    let parts = line.split(separator: " ")
    guard parts.count == 2 else { return nil }
    
    let action = String(parts[0])
    let ranges = parts[1].split(separator: ",").map { String($0) }
    
    let xRange = ranges[0].dropFirst(2).split(separator: "..").compactMap { Int($0) }
    let yRange = ranges[1].dropFirst(2).split(separator: "..").compactMap { Int($0) }
    let zRange = ranges[2].dropFirst(2).split(separator: "..").compactMap { Int($0) }
    
    guard xRange.count == 2, yRange.count == 2, zRange.count == 2 else { return nil }
    
    return RebootStep(action: action, xStart: xRange[0], xEnd: xRange[1], yStart: yRange[0], yEnd: yRange[1], zStart: zRange[0], zEnd: zRange[1])
}

func executeRebootSteps(_ cubeGrid: inout [[[Bool]]], _ rebootSteps: [RebootStep]) {
    for step in rebootSteps {
        guard step.xStart >= -50, step.xEnd <= 50, step.yStart >= -50, step.yEnd <= 50, step.zStart >= -50, step.zEnd <= 50 else { continue }
        
        for x in step.xStart...step.xEnd {
            for y in step.yStart...step.yEnd {
                for z in step.zStart...step.zEnd {
                    cubeGrid[x + 50][y + 50][z + 50] = (step.action == "on")
                }
            }
        }
    }
}

func countOnCubes(_ cubeGrid: [[[Bool]]]) -> Int {
    return cubeGrid.flatMap { $0 }.flatMap { $0 }.filter { $0 }.count
}

main()

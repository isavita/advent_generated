
import Foundation

struct Cube {
    var isOn: Bool
    var x1, x2, y1, y2, z1, z2: Int
    
    func getIntersection(with other: Cube) -> (Cube, Bool) {
        let x1 = max(self.x1, other.x1)
        let x2 = min(self.x2, other.x2)
        let y1 = max(self.y1, other.y1)
        let y2 = min(self.y2, other.y2)
        let z1 = max(self.z1, other.z1)
        let z2 = min(self.z2, other.z2)
        
        if x1 > x2 || y1 > y2 || z1 > z2 {
            return (Cube(isOn: false, x1: 0, x2: 0, y1: 0, y2: 0, z1: 0, z2: 0), false)
        }
        
        let intersectionState: Bool
        if self.isOn && other.isOn {
            intersectionState = false
        } else if !self.isOn && !other.isOn {
            intersectionState = true
        } else {
            intersectionState = other.isOn
        }
        
        return (Cube(isOn: intersectionState, x1: x1, x2: x2, y1: y1, y2: y2, z1: z1, z2: z2), true)
    }
    
    func volume() -> Int {
        let vol = (x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1)
        return isOn ? vol : -vol
    }
}

func parseInput(_ input: String) -> [Cube] {
    return input.split(separator: "\n").compactMap { line in
        let parts = line.split(separator: " ")
        let coords = parts[1].split(separator: ",").map { String($0) }
        let ranges = coords.map { $0.split(separator: "=")[1].split(separator: "..").compactMap { Int($0) } }
        
        guard ranges.count == 3, ranges.allSatisfy({ $0.count == 2 }) else { return nil }
        
        return Cube(
            isOn: parts[0] == "on",
            x1: ranges[0][0], x2: ranges[0][1],
            y1: ranges[1][0], y2: ranges[1][1],
            z1: ranges[2][0], z2: ranges[2][1]
        )
    }
}

func solve(_ input: String) -> Int {
    let cubes = parseInput(input)
    var finalList: [Cube] = []
    
    for cube in cubes {
        var toAdd: [Cube] = []
        
        for finalCube in finalList {
            let (intersection, didIntersect) = finalCube.getIntersection(with: cube)
            if didIntersect {
                toAdd.append(intersection)
            }
        }
        
        if cube.isOn {
            toAdd.append(cube)
        }
        
        finalList.append(contentsOf: toAdd)
    }
    
    return finalList.reduce(0) { $0 + $1.volume() }
}

func main() {
    do {
        let input = try String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines)
        let result = solve(input)
        print(result)
    } catch {
        print("Error reading file: \(error)")
    }
}

main()

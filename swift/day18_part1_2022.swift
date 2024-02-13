
import Foundation

let input = try String(contentsOfFile: "input.txt")
let lines = input.components(separatedBy: .newlines)

var cubes = Set<String>()

for line in lines {
    let coords = line.components(separatedBy: ",").map { Int($0)! }
    cubes.insert("\(coords[0]),\(coords[1]),\(coords[2])")
}

var surfaceArea = 0

for cube in cubes {
    let coords = cube.components(separatedBy: ",").map { Int($0)! }
    let x = coords[0]
    let y = coords[1]
    let z = coords[2]
    
    var sides = 0
    
    if !cubes.contains("\(x+1),\(y),\(z)") {
        sides += 1
    }
    if !cubes.contains("\(x-1),\(y),\(z)") {
        sides += 1
    }
    if !cubes.contains("\(x),\(y+1),\(z)") {
        sides += 1
    }
    if !cubes.contains("\(x),\(y-1),\(z)") {
        sides += 1
    }
    if !cubes.contains("\(x),\(y),\(z+1)") {
        sides += 1
    }
    if !cubes.contains("\(x),\(y),\(z-1)") {
        sides += 1
    }
    
    surfaceArea += sides
}

print(surfaceArea)

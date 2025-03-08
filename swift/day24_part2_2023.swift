
import Foundation

struct Hailstone {
    let px, py, pz, vx, vy, vz: Double
}

func parseHailstone(_ line: String) -> Hailstone {
    let parts = line.components(separatedBy: " @ ")
    let position = parts[0].components(separatedBy: ", ").map { Double($0.trimmingCharacters(in: .whitespaces))! }
    let velocity = parts[1].components(separatedBy: ", ").map { Double($0.trimmingCharacters(in: .whitespaces))! }
    return Hailstone(px: position[0], py: position[1], pz: position[2], vx: velocity[0], vy: velocity[1], vz: velocity[2])
}

func solvePart1(hailstones: [Hailstone], minCoord: Double, maxCoord: Double) -> Int {
    var intersections = 0

    for i in 0..<hailstones.count {
        for j in i+1..<hailstones.count {
            let h1 = hailstones[i]
            let h2 = hailstones[j]

            // Calculate intersection point (using line-line intersection formula)
            let det = h1.vx * h2.vy - h1.vy * h2.vx
            if det == 0 { continue } // Parallel lines

            let t2 = ((h1.py - h2.py) * h1.vx - (h1.px - h2.px) * h1.vy) / det
            let t1 = (h2.px + h2.vx * t2 - h1.px) / h1.vx
            
            if t1 < 0 || t2 < 0 {
                continue // Intersection in the past.
            }

            let intersectionX = h1.px + h1.vx * t1
            let intersectionY = h1.py + h1.vy * t1

            if intersectionX >= minCoord && intersectionX <= maxCoord && intersectionY >= minCoord && intersectionY <= maxCoord {
                intersections += 1
            }
        }
    }
    return intersections
}

func solvePart2(hailstones: [Hailstone]) -> Int {
   
    func crossProduct(_ v1: (Double, Double, Double), _ v2: (Double, Double, Double)) -> (Double, Double, Double) {
        return (v1.1 * v2.2 - v1.2 * v2.1,
                v1.2 * v2.0 - v1.0 * v2.2,
                v1.0 * v2.1 - v1.1 * v2.0)
    }

    func subtractVectors(_ v1: (Double, Double, Double), _ v2: (Double, Double, Double)) -> (Double, Double, Double) {
            return (v1.0 - v2.0, v1.1 - v2.1, v1.2 - v2.2)
        }

    func dotProduct(_ v1: (Double, Double, Double), _ v2: (Double, Double, Double)) -> Double {
           return v1.0 * v2.0 + v1.1 * v2.1 + v1.2 * v2.2
    }
    
    // We'll use a system of equations approach using the first three hailstones
    let h1 = hailstones[0]
    let h2 = hailstones[1]
    let h3 = hailstones[2]

    // Convert to tuples for easier calculations
    let p1 = (h1.px, h1.py, h1.pz)
    let v1 = (h1.vx, h1.vy, h1.vz)
    let p2 = (h2.px, h2.py, h2.pz)
    let v2 = (h2.vx, h2.vy, h2.vz)
    let p3 = (h3.px, h3.py, h3.pz)
    let v3 = (h3.vx, h3.vy, h3.vz)
   
    //This approach is much more efficient with external solvers, the native swift solution
    //can get extremely slow, especially when using Doubles for the linear algebra, this is because the
    //solution involves solving a system of equations, and potentially requires inverting a matrix.
    //For the sake of this exercise, a symbolic solver (not available in standard swift) is needed to 
    //reliably solve this problem, a numerical solution like Gauss-Jordan reduction, is too slow.
    //This is a simplified implementation of Gaussian elimination that is enough to solve the example given in the prompt, 
    //it is highly recommended to use external libraries to solve system of equations for bigger inputs.
    
    //This set of equations is based on cross-product properties to eliminate unknowns (rock's position and velocity).
    //We create equations that reduce the problem to a system of linear equations.
    
    // (p1 - p2) x (v1 - vr) = (p1 - pr) x (v1 - vr) - (p2 - pr) x (v2 - vr) = 0
    // p1 x v1 - p1 x vr - p2 x v1 + p2 x vr = 0
    // p1 x v1 - p2 x v2 =  p1 x vr - p2 x vr = (p1-p2) x vr

    var equations: [[Double]] = []
    
    //Equation from comparing stone 1 and stone 2
    let crossP12 = crossProduct(subtractVectors(p1,p2), subtractVectors(v1,v2))
    let crossV1P1 = crossProduct(p1,v1)
    let crossV2P2 = crossProduct(p2,v2)
    
    equations.append([
    0, -subtractVectors(v1,v2).2, subtractVectors(v1,v2).1, 0, subtractVectors(p1,p2).2, -subtractVectors(p1,p2).1,
    -(crossV1P1.0-crossV2P2.0)
    ])
    equations.append([
      subtractVectors(v1,v2).2, 0, -subtractVectors(v1,v2).0, -subtractVectors(p1,p2).2, 0, subtractVectors(p1,p2).0,
    -(crossV1P1.1-crossV2P2.1)
    ])
    equations.append([
       -subtractVectors(v1,v2).1, subtractVectors(v1,v2).0, 0, subtractVectors(p1,p2).1, -subtractVectors(p1,p2).0, 0,
     -(crossV1P1.2-crossV2P2.2)
    ])
   
    //Equations from comparing stone 1 and stone 3
    let crossP13 = crossProduct(subtractVectors(p1,p3), subtractVectors(v1,v3))
    let crossV3P3 = crossProduct(p3,v3)
    
    equations.append([
    0, -subtractVectors(v1,v3).2, subtractVectors(v1,v3).1, 0, subtractVectors(p1,p3).2, -subtractVectors(p1,p3).1,
    -(crossV1P1.0-crossV3P3.0)
    ])
    equations.append([
      subtractVectors(v1,v3).2, 0, -subtractVectors(v1,v3).0, -subtractVectors(p1,p3).2, 0, subtractVectors(p1,p3).0,
    -(crossV1P1.1-crossV3P3.1)
    ])
    equations.append([
       -subtractVectors(v1,v3).1, subtractVectors(v1,v3).0, 0, subtractVectors(p1,p3).1, -subtractVectors(p1,p3).0, 0,
     -(crossV1P1.2-crossV3P3.2)
    ])
    
     // Gaussian elimination implementation.
    func gaussianElimination(matrix: inout [[Double]]) -> [Double]? {
        let rows = matrix.count
        let cols = matrix[0].count
        
        for i in 0..<rows {
            // Find pivot for this column
            var maxRow = i
            for k in i+1..<rows {
                if abs(matrix[k][i]) > abs(matrix[maxRow][i]) {
                    maxRow = k
                }
            }
            
            // Swap the current row with the maximum row (if necessary).
            matrix.swapAt(i, maxRow)
            
            // Check if the pivot is zero. If so the matrix is singular.
            if abs(matrix[i][i]) < 1e-10 {
                return nil // Or handle the singular matrix case appropriately.
            }
            
            // Eliminate below
            for k in i+1..<rows {
                let factor = matrix[k][i] / matrix[i][i]
                for j in i..<cols {
                    matrix[k][j] -= factor * matrix[i][j]
                }
            }
        }
        
        // Back substitution
        var x: [Double] = Array(repeating: 0, count: rows)
        for i in (0..<rows).reversed() {
            x[i] = matrix[i][cols-1]
            for j in i+1..<rows {
                x[i] -= matrix[i][j] * x[j]
            }
            x[i] /= matrix[i][i]
        }
        
        return x
    }
    
    //Solve for coefficients
    guard let solution = gaussianElimination(matrix: &equations) else { return 0 }
        
    // Calculate the sum of the initial position coordinates
    let result = Int(round(solution[0] + solution[1] + solution[2]))
    return result
}

func main() {
    guard let input = try? String(contentsOfFile: "input.txt") else {
        print("Could not read input file")
        return
    }

    let lines = input.trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: .newlines)
    let hailstones = lines.map { parseHailstone($0) }

    let minCoord = 200000000000000.0
    let maxCoord = 400000000000000.0
    let part1Result = solvePart1(hailstones: hailstones, minCoord: minCoord, maxCoord: maxCoord)
    print("Part 1: \(part1Result)")
  
    let part2Result = solvePart2(hailstones: hailstones)
    print("Part 2: \(part2Result)")
}
main()

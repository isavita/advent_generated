
import Foundation

let input = try String(contentsOfFile: "input.txt").components(separatedBy: "\n")
let earliestTimestamp = Int(input[0])!
let busIDs = input[1].components(separatedBy: ",").filter { $0 != "x" }.compactMap { Int($0) }

let (busID, waitTime) = busIDs.map { ($0, $0 - (earliestTimestamp % $0)) }.min { $0.1 < $1.1 }!

print(busID * waitTime)

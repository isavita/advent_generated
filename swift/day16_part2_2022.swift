
import Foundation

struct Valve {
    let flowRate: Int
    let tunnels: [String]
}

func parseInput(from file: String) -> [String: Valve] {
    var valves: [String: Valve] = [:]
    
    guard let data = try? String(contentsOfFile: file) else {
        fatalError("Could not read from file \(file)")
    }

    let lines = data.components(separatedBy: .newlines).filter { !$0.isEmpty }
    for line in lines {
        let components = line.components(separatedBy: "; ")
        let valvePart = components[0].components(separatedBy: " ")
        let valveName = valvePart[1]
        let flowRate = Int(valvePart[4].components(separatedBy: "=")[1])!
        
        let tunnelsPart = components[1].hasPrefix("tunnel leads to valve ") ?
        components[1].dropFirst("tunnel leads to valve ".count) :
        components[1].dropFirst("tunnels lead to valves ".count)
        
        
        let tunnels = String(tunnelsPart).components(separatedBy: ", ")
        valves[valveName] = Valve(flowRate: flowRate, tunnels: tunnels)
    }
    return valves
}

func solve(valves: [String: Valve], minutes: Int, elephant: Bool) -> Int {
    let distances = computeDistances(valves: valves)
    let usefulValves = valves.filter { $0.value.flowRate > 0 }.keys
    var maxPressure = 0
    
    func dfs(currentValve: String, timeRemaining: Int, openedValves: Set<String>, currentPressure: Int, path: [(String, Int)]) {
          maxPressure = max(maxPressure, currentPressure)
        
          for nextValve in usefulValves where !openedValves.contains(nextValve) {
              let travelTime = distances[currentValve]![nextValve]! + 1 // +1 to open the valve
              if timeRemaining >= travelTime {
                let newTimeRemaining = timeRemaining - travelTime
                let newPressure = currentPressure + newTimeRemaining * valves[nextValve]!.flowRate
                  dfs(currentValve: nextValve, timeRemaining: newTimeRemaining, openedValves: openedValves.union([nextValve]), currentPressure: newPressure, path: path + [(nextValve,newTimeRemaining)])
              }
          }
      }
    
    
    if !elephant{
        dfs(currentValve: "AA", timeRemaining: minutes, openedValves: [], currentPressure: 0, path: [])
        return maxPressure
    } else {
        var best = 0
        let usefulValvesArray = Array(usefulValves)
        
        // Iterate through all possible combinations of valves that I could open
           for i in 0..<(1 << usefulValvesArray.count) {
               var myValves: Set<String> = []
               for j in 0..<usefulValvesArray.count {
                   if (i >> j) & 1 == 1 {
                       myValves.insert(usefulValvesArray[j])
                   }
               }

               // Calculate max pressure for my valves
               var myMaxPressure = 0
               func myDFS(currentValve: String, timeRemaining: Int, openedValves: Set<String>, currentPressure: Int) {
                   myMaxPressure = max(myMaxPressure, currentPressure)
                   for nextValve in myValves where !openedValves.contains(nextValve) {
                       let travelTime = distances[currentValve]![nextValve]! + 1
                       if timeRemaining >= travelTime {
                           let newTimeRemaining = timeRemaining - travelTime
                           let newPressure = currentPressure + newTimeRemaining * valves[nextValve]!.flowRate
                           myDFS(currentValve: nextValve, timeRemaining: newTimeRemaining, openedValves: openedValves.union([nextValve]), currentPressure: newPressure)
                       }
                   }
               }
               myDFS(currentValve: "AA", timeRemaining: 26, openedValves: [], currentPressure: 0)
                

               // Calculate max pressure for elephant's valves (complement of my valves)
               var elephantValves = Set(usefulValves).subtracting(myValves)
               var elephantMaxPressure = 0
             func elephantDFS(currentValve: String, timeRemaining: Int, openedValves: Set<String>, currentPressure: Int) {
                 elephantMaxPressure = max(elephantMaxPressure, currentPressure)
                 
                 for nextValve in elephantValves where !openedValves.contains(nextValve) {
                     let travelTime = distances[currentValve]![nextValve]! + 1
                     if timeRemaining >= travelTime {
                         let newTimeRemaining = timeRemaining - travelTime
                         let newPressure = currentPressure + newTimeRemaining * valves[nextValve]!.flowRate
                         elephantDFS(currentValve: nextValve, timeRemaining: newTimeRemaining, openedValves: openedValves.union([nextValve]), currentPressure: newPressure)
                     }
                 }
             }
             elephantDFS(currentValve: "AA", timeRemaining: 26, openedValves: [], currentPressure: 0)

               best = max(best, myMaxPressure + elephantMaxPressure)
           }

           return best
        
    }
    

}

func computeDistances(valves: [String: Valve]) -> [String: [String: Int]] {
    var distances: [String: [String: Int]] = [:]
    for valve in valves.keys {
        distances[valve] = [:]
        for otherValve in valves.keys {
            distances[valve]![otherValve] = (valve == otherValve) ? 0 : Int.max / 2 //Prevent integer overflow
        }
    }

    for valve in valves.keys {
        for neighbor in valves[valve]!.tunnels {
            distances[valve]![neighbor] = 1
        }
    }
    
    //Floyd-Warshall
    for k in valves.keys {
          for i in valves.keys {
              for j in valves.keys {
                  if distances[i]![j]! > distances[i]![k]! + distances[k]![j]! {
                      distances[i]![j]! = distances[i]![k]! + distances[k]![j]!
                  }
              }
          }
      }

    return distances
}

func main() {
    let valves = parseInput(from: "input.txt")

    let part1Result = solve(valves: valves, minutes: 30, elephant: false)
    print("Part 1: \(part1Result)")

    let part2Result = solve(valves: valves, minutes: 26, elephant: true)
     print("Part 2: \(part2Result)")
}

main()

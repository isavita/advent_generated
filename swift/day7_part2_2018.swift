
import Foundation

struct Step {
    let name: String
    var prerequisites: Set<String>
}

func solvePartOne(steps: [String: Step]) -> String {
    var steps = steps
    var result = ""
    var availableSteps: [String] = []

    while !steps.isEmpty || !availableSteps.isEmpty {
        // Find available steps
        availableSteps = []
        for (name, step) in steps {
            if step.prerequisites.isEmpty {
                availableSteps.append(name)
            }
        }
        availableSteps.sort()
        
        guard let nextStepName = availableSteps.first else {
             break; // Should not happen in a well-formed input, but handle it gracefully.
        }

        result += nextStepName
        steps.removeValue(forKey: nextStepName)

        // Remove the completed step from prerequisites of other steps
        for (name, var step) in steps {
            step.prerequisites.remove(nextStepName)
            steps[name] = step
        }
    }

    return result
}

func solvePartTwo(steps: [String: Step], workerCount: Int, baseDuration: Int) -> Int {
    var steps = steps
    var workers: [(step: String, timeLeft: Int)?] = Array(repeating: nil, count: workerCount)
    var time = 0
    var completedSteps: Set<String> = []
    var availableSteps: [String] = []
    
    while !steps.isEmpty || workers.contains(where: { $0 != nil })  {

        // 1. Find available steps *considering completed steps*
        availableSteps = []
        for (name, step) in steps {
            if step.prerequisites.isSubset(of: completedSteps) { // Check if ALL prerequisites are done
                availableSteps.append(name)
            }
        }
        availableSteps.sort()

        // 2. Assign tasks to idle workers
        for i in 0..<workers.count {
            if workers[i] == nil, let nextStepName = availableSteps.first {
                availableSteps.removeFirst()
                let duration = baseDuration + (Int(nextStepName.unicodeScalars.first!.value) - Int(UnicodeScalar("A").value)) + 1
                workers[i] = (nextStepName, duration)
                steps.removeValue(forKey: nextStepName)  // Remove from steps *here*, after assignment
            }
        }

        // 3. Advance time by one unit and process workers
        time += 1

        for i in 0..<workers.count {
            if var worker = workers[i] {
                worker.timeLeft -= 1
                if worker.timeLeft == 0 {
                    completedSteps.insert(worker.step)
                    workers[i] = nil
                } else {
                    workers[i] = worker
                }
            }
        }
    }

    return time
}

func main() {
    do {
        let input = try String(contentsOfFile: "input.txt", encoding: .utf8)
        let lines = input.trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: .newlines)

        var steps: [String: Step] = [:]

        for line in lines {
            let components = line.components(separatedBy: " ")
            let prerequisite = components[1]
            let stepName = components[7]

            // Initialize or update the steps
            if steps[stepName] == nil {
                steps[stepName] = Step(name: stepName, prerequisites: [])
            }
            steps[stepName]!.prerequisites.insert(prerequisite)

            if steps[prerequisite] == nil {
                steps[prerequisite] = Step(name: prerequisite, prerequisites: [])
            }
        }

        let part1Result = solvePartOne(steps: steps)
        print("Part 1: \(part1Result)")
        
        //Re-read and parse for part 2
        var steps2: [String: Step] = [:]

        for line in lines {
            let components = line.components(separatedBy: " ")
            let prerequisite = components[1]
            let stepName = components[7]
            if steps2[stepName] == nil {
                steps2[stepName] = Step(name: stepName, prerequisites: [])
            }
            steps2[stepName]!.prerequisites.insert(prerequisite)
            if steps2[prerequisite] == nil {
                steps2[prerequisite] = Step(name: prerequisite, prerequisites: [])
            }
        }

        let part2Result = solvePartTwo(steps: steps2, workerCount: 5, baseDuration: 60)
        print("Part 2: \(part2Result)")

    } catch {
        print("Error reading input file: \(error)")
    }
}

main()

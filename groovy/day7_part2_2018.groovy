
import java.util.regex.Matcher
import java.util.regex.Pattern

def solvePart1(List<String> instructions) {
    def dependencies = [:]
    def allSteps = new HashSet<>()

    Pattern pattern = Pattern.compile("Step (\\w) must be finished before step (\\w) can begin.")

    instructions.each { instruction ->
        Matcher matcher = pattern.matcher(instruction)
        if (matcher.find()) {
            def prerequisite = matcher.group(1)
            def step = matcher.group(2)

            dependencies.computeIfAbsent(step, { new HashSet<>() }).add(prerequisite)
            allSteps.add(prerequisite)
            allSteps.add(step)
        }
    }

    def completed = []
    def availableSteps = allSteps.findAll { step -> !dependencies.containsKey(step) } as SortedSet

    while (!availableSteps.isEmpty()) {
        def nextStep = availableSteps.first()
        availableSteps.remove(nextStep)
        completed.add(nextStep)

        dependencies.each { step, prerequisites ->
            prerequisites.remove(nextStep)
            if (prerequisites.isEmpty()) {
                availableSteps.add(step)
            }
        }

        dependencies = dependencies.findAll { step, prerequisites -> !prerequisites.isEmpty() }
    }

    return completed.join()
}

def solvePart2(List<String> instructions, int numWorkers, int baseTime) {
    def dependencies = [:]
    def allSteps = new HashSet<>()

    Pattern pattern = Pattern.compile("Step (\\w) must be finished before step (\\w) can begin.")

    instructions.each { instruction ->
        Matcher matcher = pattern.matcher(instruction)
        if (matcher.find()) {
            def prerequisite = matcher.group(1)
            def step = matcher.group(2)

            dependencies.computeIfAbsent(step, { new HashSet<>() }).add(prerequisite)
            allSteps.add(prerequisite)
            allSteps.add(step)
        }
    }

    def completed = []
    def availableSteps = allSteps.findAll { step -> !dependencies.containsKey(step) } as SortedSet
    def workers = (1..numWorkers).collect { [step: null, timeRemaining: 0] }
    int time = 0

    while (completed.size() < allSteps.size()) {
        workers.eachWithIndex { worker, index ->
            if (worker.step == null && !availableSteps.isEmpty()) {
                def nextStep = availableSteps.first()
                availableSteps.remove(nextStep)
                worker.step = nextStep
                worker.timeRemaining = baseTime + (nextStep.charAt(0) - 'A'.charAt(0) + 1)
            }
        }

        workers.each { worker ->
            if (worker.step != null) {
                worker.timeRemaining--
                if (worker.timeRemaining == 0) {
                    completed.add(worker.step)
                    def completedStep = worker.step
                    worker.step = null


                    dependencies.each { step, prerequisites ->
                        prerequisites.remove(completedStep)
                    }

                    availableSteps.addAll(dependencies.findAll { step, prerequisites -> prerequisites.isEmpty() }.keySet())
                    dependencies = dependencies.findAll { step, prerequisites -> !prerequisites.isEmpty() }
                    availableSteps = availableSteps.intersect(allSteps) as SortedSet

                }
            }
        }


        time++
        def stepsBeingWorkedOn = workers.findAll{it.step != null}.collect{it.step}
        def possibleSteps = allSteps - completed - stepsBeingWorkedOn

        def availableNow = possibleSteps.findAll{ step ->
            !dependencies.containsKey(step) || (dependencies.get(step).size() == 0)
        }
        availableSteps.addAll(availableNow as SortedSet)
        availableSteps = availableSteps.intersect(allSteps) as SortedSet

    }

    return time
}

def main() {
    def instructions = new File("input.txt").readLines()

    println "Part 1: ${solvePart1(instructions)}"
    println "Part 2: ${solvePart2(instructions, 5, 60)}"
}

main()

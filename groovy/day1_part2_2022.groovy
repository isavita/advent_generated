
def inputFile = new File("input.txt")
def lines = inputFile.readLines()

def elvesCalories = []
def currentElfCalories = 0

for (line in lines) {
    if (line.isEmpty()) {
        elvesCalories.add(currentElfCalories)
        currentElfCalories = 0
    } else {
        currentElfCalories += line.toInteger()
    }
}
elvesCalories.add(currentElfCalories)

def sortedElvesCalories = elvesCalories.sort { -it }

def partOneAnswer = sortedElvesCalories[0]

def partTwoAnswer = sortedElvesCalories[0] + sortedElvesCalories[1] + sortedElvesCalories[2]

println(partTwoAnswer)

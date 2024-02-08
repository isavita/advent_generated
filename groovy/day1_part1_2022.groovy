def input = new File("input.txt").readLines()

def maxCalories = 0
def currentCalories = 0

for (line in input) {
    if (line.isEmpty()) {
        if (currentCalories > maxCalories) {
            maxCalories = currentCalories
        }
        currentCalories = 0
    } else {
        currentCalories += line.toInteger()
    }
}

if (currentCalories > maxCalories) {
    maxCalories = currentCalories
}

println maxCalories
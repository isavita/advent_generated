
def frequencies = []
def currentFrequency = 0
def foundDuplicate = false

def input = new File("input.txt").readLines()

while (!foundDuplicate) {
    input.each { line ->
        currentFrequency += line.toInteger()
        if (frequencies.contains(currentFrequency)) {
            println(currentFrequency)
            foundDuplicate = true
        } else {
            frequencies.add(currentFrequency)
        }
    }
}

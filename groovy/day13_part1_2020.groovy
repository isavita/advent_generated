
def file = new File("input.txt")
def lines = file.readLines()
def earliestDeparture = lines[0] as int
def busIDs = lines[1].split(",")

def earliestBusID = 0
def minWaitTime = earliestDeparture

busIDs.each { id ->
    if (id == "x") {
        return
    }
    def busID = id as int
    def waitTime = busID - (earliestDeparture % busID)
    if (waitTime < minWaitTime) {
        minWaitTime = waitTime
        earliestBusID = busID
    }
}

println(earliestBusID * minWaitTime)

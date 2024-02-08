
def happinessMap = readHappinessValues("input.txt")
addYourself(happinessMap)

def guests = getGuestList(happinessMap)
def maxHappiness = calculateOptimalArrangement(guests, happinessMap)
println maxHappiness

Map readHappinessValues(String filename) {
    def happinessMap = [:]
    new File(filename).eachLine { line ->
        def parts = line.tokenize()
        if (parts.size() < 11) {
            return
        }
        def from = parts[0]
        def to = parts[10][0..-2]
        def change = parts[3].toInteger()
        if (parts[2] == "lose") {
            change = -change
        }

        if (!happinessMap.containsKey(from)) {
            happinessMap[from] = [:]
        }
        happinessMap[from][to] = change
    }
    return happinessMap
}

void addYourself(Map happinessMap) {
    happinessMap["You"] = [:]
    happinessMap.each { guest, _ ->
        happinessMap[guest]["You"] = 0
        happinessMap["You"][guest] = 0
    }
}

List getGuestList(Map happinessMap) {
    return happinessMap.keySet() as List
}

int calculateOptimalArrangement(List guests, Map happinessMap) {
    def maxHappiness = [0]
    permute(guests, 0, maxHappiness, happinessMap)
    return maxHappiness[0]
}

void permute(List arr, int i, List maxHappiness, Map happinessMap) {
    if (i > arr.size()) {
        return
    }
    if (i == arr.size()) {
        def happiness = calculateHappiness(arr, happinessMap)
        if (happiness > maxHappiness[0]) {
            maxHappiness[0] = happiness
        }
        return
    }
    for (int j = i; j < arr.size(); j++) {
        def temp = arr[i]
        arr[i] = arr[j]
        arr[j] = temp
        permute(arr, i + 1, maxHappiness, happinessMap)
        temp = arr[i]
        arr[i] = arr[j]
        arr[j] = temp
    }
}

int calculateHappiness(List arrangement, Map happinessMap) {
    def happiness = 0
    def n = arrangement.size()
    arrangement.eachWithIndex { guest, i ->
        def left = (i + n - 1) % n
        def right = (i + 1) % n
        happiness += happinessMap[guest][arrangement[left]]
        happiness += happinessMap[guest][arrangement[right]]
    }
    return happiness
}

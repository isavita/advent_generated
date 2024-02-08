
def happinessMap = [:]

def input = new File("input.txt").readLines()

input.each {
    def parts = it.split(" ")
    def person1 = parts[0]
    def person2 = parts[10].substring(0, parts[10].length() - 1)
    def happiness = Integer.parseInt(parts[3])
    if (parts[2] == "lose") {
        happiness *= -1
    }

    happinessMap["${person1}_${person2}"] = happiness
}

def people = happinessMap.keySet().collect {
    it.split("_")[0]
}.unique()

def maxHappiness = 0

people.permutations().each {
    def happiness = 0
    for (int i = 0; i < it.size(); i++) {
        def person1 = it[i]
        def person2 = i == it.size() - 1 ? it[0] : it[i + 1]
        happiness += happinessMap["${person1}_${person2}"]
        happiness += happinessMap["${person2}_${person1}"]
    }
    maxHappiness = Math.max(maxHappiness, happiness)
}

println(maxHappiness)

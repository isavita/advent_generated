def input = new File("input.txt").text.trim()

def react(polymer) {
    def result = []

    polymer.each { unit ->
        if (result && unit.toUpperCase() == result[-1].toUpperCase() && unit != result[-1]) {
            result.removeAt(result.size() - 1)
        } else {
            result.add(unit)
        }
    }

    return result.join("")
}

def fullyReactedPolymer = react(input)

def lengths = ('a'..'z').collect { unit ->
    react(fullyReactedPolymer.replaceAll("(?i)$unit", "")).size()
}

println lengths.min()
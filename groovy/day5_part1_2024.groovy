
import java.util.regex.Matcher
import java.util.regex.Pattern

def solve() {
    def (orderingRules, updates) = readInput("input.txt")

    def sum = 0
    updates.each { update ->
        if (isCorrectlyOrdered(update, orderingRules)) {
            sum += update[update.size() / 2]
        }
    }

    println sum
}

def readInput(filename) {
    def orderingRules = []
    def updates = []
    def isUpdateSection = false

    new File(filename).eachLine { line ->
        line = line.trim()
        if (line.isEmpty()) {
            isUpdateSection = true
            return
        }

        if (!isUpdateSection) {
            def (x, y) = line.split("\\|").collect { it.trim() }*.toInteger()
            orderingRules << [x, y]
        } else {
            def update = line.split(",").collect { it.trim() }*.toInteger()
            updates << update
        }
    }

    return [orderingRules, updates]
}

def isCorrectlyOrdered(update, rules) {
    def position = [:]
    update.eachWithIndex { page, idx -> position[page] = idx }

    rules.every { rule ->
        def x = rule[0]
        def y = rule[1]
        def posX = position.get(x)
        def posY = position.get(y)
        !(posX != null && posY != null && posX >= posY)
    }
}


solve()

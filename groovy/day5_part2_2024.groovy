
def solve() {
    def (orderingRules, updates) = readInput("input.txt")
    def sum = 0
    updates.each { update ->
        if (!isCorrectlyOrdered(update, orderingRules)) {
            def sortedUpdate = sortUpdate(update, orderingRules)
            if (sortedUpdate) {
                sum += sortedUpdate[sortedUpdate.size() / 2]
            }
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
        if (!line) {
            isUpdateSection = true
            return
        }
        if (!isUpdateSection) {
            def parts = line.split("\\|")
            if (parts.size() == 2) {
                try {
                    orderingRules << [parts[0].trim().toInteger(), parts[1].trim().toInteger()]
                } catch (NumberFormatException ignored) {
                }
            }
        } else {
            def nums = line.split(",")
            def update = nums.collect { it.trim() }.findAll { it }.collect {
                try {
                    it.toInteger()
                } catch (NumberFormatException ignored) {
                    null
                }
            }.findAll { it != null }
            if (update) {
                updates << update
            }
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
        if (position.containsKey(x) && position.containsKey(y)) {
            if (position[x] >= position[y]) {
                return false
            }
        }
        true
    }
}

def sortUpdate(update, rules) {
    def adjacency = [:]
    def pagesInUpdate = update.toSet()
    pagesInUpdate.each { page -> adjacency[page] = [] }
    rules.each { rule ->
        def x = rule[0]
        def y = rule[1]
        if (pagesInUpdate.contains(x) && pagesInUpdate.contains(y)) {
            adjacency[x] << y
        }
    }
    def visited = [:]
    def tempMarked = [:]
    def result = []
    def visit
    visit = { n ->
        if (tempMarked[n]) {
            throw new Exception("Cycle detected")
        }
        if (!visited[n]) {
            tempMarked[n] = true
            adjacency[n].each { m ->
                visit(m)
            }
            tempMarked[n] = false
            visited[n] = true
            result << n
        }
    }
    pagesInUpdate.each { page ->
        if (!visited[page]) {
            try {
                visit(page)
            } catch (Exception e) {
                return null
            }
        }
    }
    result.reverse()
    return result
}

solve()


class Rule {
    String name
    List<List<Integer>> ranges
    boolean isValid(int value) {
        ranges.any { low, high -> value >= low && value <= high }
    }
}

static List<Integer> parseTicket(String s) {
    s.split(',').collect { it.toInteger() }
}

static boolean isValidTicket(List<Integer> ticket, List<Rule> rules) {
    ticket.every { value -> rules.any { it.isValid(value) } }
}

static Map<String, Integer> solveFieldPositions(List<Rule> rules, List<List<Integer>> tickets) {
    def validPositions = rules.collectEntries { rule -> [rule.name, (0..<tickets[0].size()).toSet()] }
    tickets.each { ticket ->
        ticket.eachWithIndex { value, idx ->
            rules.each { rule ->
                if (!rule.isValid(value)) validPositions[rule.name].remove(idx)
            }
        }
    }

    def fieldPositions = [:]
    while (fieldPositions.size() < rules.size()) {
        validPositions.findAll { it.value.size() == 1 }.each { name, positions ->
            def pos = positions.first()
            fieldPositions[name] = pos
            validPositions.values().each { it.remove(pos) }
            validPositions.remove(name)
        }
    }
    fieldPositions
}

static long calculateDepartureProduct(List<Integer> ticket, Map<String, Integer> fieldPositions) {
    fieldPositions.findAll { it.key.startsWith("departure") }.collect { it.value }.inject(1L) { acc, pos -> acc * ticket[pos] }
}

static void main(String[] args) {
    def lines = new File("input.txt").readLines()
    def rules = []
    def myTicket = []
    def nearbyTickets = []
    def section = 0
    def reRule = ~/([^:]+): (\d+)-(\d+) or (\d+)-(\d+)/

    lines.each { line ->
        if (line.isEmpty()) {
            section++
            return
        }
        if (section == 0) {
            def match = reRule.matcher(line)
            if (match.matches()) {
                rules << new Rule(name: match[0][1], ranges: [[match[0][2].toInteger(), match[0][3].toInteger()], [match[0][4].toInteger(), match[0][5].toInteger()]])
            }
        } else if (section == 1 && line != "your ticket:") {
            myTicket = parseTicket(line)
        } else if (section == 2 && line != "nearby tickets:") {
            def ticket = parseTicket(line)
            if (isValidTicket(ticket, rules)) nearbyTickets << ticket
        }
    }

    def fieldPositions = solveFieldPositions(rules, nearbyTickets)
    println calculateDepartureProduct(myTicket, fieldPositions)
}

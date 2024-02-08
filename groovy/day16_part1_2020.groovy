
def rules = [:]
def nearbyTickets = []

new File('input.txt').eachLine { line ->
    if (line.contains('-')) {
        def (field, ranges) = line.tokenize(':')
        rules[field] = ranges.tokenize('or').collect { it.tokenize('-').collect { it.toInteger() } as List }
    } else if (line.contains(',')) {
        nearbyTickets << line.tokenize(',').collect { it.toInteger() }
    }
}

def errorRate = 0
nearbyTickets.each { ticket ->
    ticket.each { value ->
        def valid = rules.any { rule ->
            rule.value.any { range ->
                value >= range[0] && value <= range[1]
            }
        }
        if (!valid) {
            errorRate += value
        }
    }
}

println errorRate

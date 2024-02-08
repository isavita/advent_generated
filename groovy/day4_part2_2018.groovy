
def inputFile = new File("input.txt")
def records = []
def guards = [:]

inputFile.eachLine { line ->
    def t = Date.parse("yyyy-MM-dd HH:mm", line[1..17])
    records.add([time: t, event: line[19..-1]])
}

records.sort { a, b -> a.time <=> b.time }

def guard
def sleepStart

records.each { record ->
    switch(record.event) {
        case { it.contains("begins shift") }:
            def id = record.event.tokenize(" ")[1][1..-1] as Integer
            if (!guards.containsKey(id)) {
                guards[id] = [id: id, minutes: new int[60], totalMin: 0]
            }
            guard = guards[id]
            break
        case { it.contains("falls asleep") }:
            sleepStart = record.time.minutes
            break
        case { it.contains("wakes up") }:
            (sleepStart..<record.time.minutes).each { i ->
                guard.minutes[i]++
                guard.totalMin++
            }
            break
    }
}

def mostFreqGuard
def mostFreqMin

guards.each { id, g ->
    g.minutes.eachWithIndex { m, i ->
        if (mostFreqGuard == null || m > mostFreqGuard.minutes[mostFreqMin]) {
            mostFreqGuard = g
            mostFreqMin = i
        }
    }
}

println mostFreqGuard.id * mostFreqMin

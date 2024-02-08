
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

def records = new File("input.txt").readLines().collect {
    def parts = it.split("\\] ")
    def timePart = parts[0][1..-1]
    def actionPart = parts[1]

    def formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
    def ts = LocalDateTime.parse(timePart, formatter)

    def guardID = -1
    if (actionPart.contains("Guard")) {
        def matcher = (actionPart =~ /Guard #(\d+) begins shift/)
        guardID = matcher[0][1] as int
        actionPart = "begins shift"
    } else if (actionPart.contains("falls asleep")) {
        actionPart = "falls asleep"
    } else if (actionPart.contains("wakes up")) {
        actionPart = "wakes up"
    }

    [timestamp: ts, action: actionPart, guardID: guardID]
}

records = records.sort { a, b -> a.timestamp.compareTo(b.timestamp) }

def guardSleepMinutes = [:]
def currentGuardID = -1
def sleepStart

records.each { record ->
    switch (record.action) {
        case "begins shift":
            currentGuardID = record.guardID
            break
        case "falls asleep":
            sleepStart = record.timestamp
            break
        case "wakes up":
            if (!guardSleepMinutes.containsKey(currentGuardID)) {
                guardSleepMinutes[currentGuardID] = new int[60]
            }
            for (int i = sleepStart.getMinute(); i < record.timestamp.getMinute(); i++) {
                guardSleepMinutes[currentGuardID][i]++
            }
            break
    }
}

def maxSleep = 0
def sleepiestGuard
guardSleepMinutes.each { guardID, minutes ->
    def totalSleep = minutes.sum()
    if (totalSleep > maxSleep) {
        maxSleep = totalSleep
        sleepiestGuard = guardID
    }
}

def maxMinute = 0
def maxMinuteCount = 0
guardSleepMinutes[sleepiestGuard].eachWithIndex { count, i ->
    if (count > maxMinuteCount) {
        maxMinuteCount = count
        maxMinute = i
    }
}

println sleepiestGuard * maxMinute

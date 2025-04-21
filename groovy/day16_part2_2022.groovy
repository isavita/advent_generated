
import java.nio.file.Files
import java.nio.file.Paths
import java.util.regex.Matcher

def readAll(path) {
    return new File(path).text.trim()
}

def maxPressure(valves, currentValve, timeLeft, openableSetMask, openableValvesList, memo) {
    def memoKey = [currentValve, timeLeft, openableSetMask]
    if (memo.containsKey(memoKey)) {
        return memo[memoKey]
    }

    def maxRelief = 0

    (0..<openableValvesList.size()).each { i ->
        if (((openableSetMask >> i) & 1) == 1) {
            def nextValveId = openableValvesList[i]
            def nextValve = valves[nextValveId]

            def dist = valves[currentValve].tunnels[nextValveId]

            if (dist == null || dist >= Integer.MAX_VALUE / 2) {
                return
            }

            def remainingTime = timeLeft - dist - 1

            if (remainingTime >= 0) {
                def currentGain = remainingTime * nextValve.flow
                def futureRelief = maxPressure(valves, nextValveId, remainingTime, openableSetMask - (1 << i), openableValvesList, memo)
                maxRelief = Math.max(maxRelief, currentGain + futureRelief)
            }
        }
    }

    memo[memoKey] = maxRelief
    return maxRelief
}

void main() {
    def valves = [:]
    def valveIds = []

    def input = readAll("input.txt")
    input.split('\n').each { line ->
        def m = (line =~ /Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.*)/)
        if (m.matches()) {
            def id = m[0][1]
            def flow = m[0][2] as int
            def tunnelStr = m[0][3]

            valveIds << id

            valves[id] = [id: id, flow: flow, tunnels: [:]]
            valves[id].tunnels[id] = 0

            tunnelStr.split(', ').each { tunnelId ->
                valves[id].tunnels[tunnelId] = 1
            }
        }
    }

    def infinity = Integer.MAX_VALUE / 2

    valveIds.each { i ->
        valveIds.each { j ->
            if (i != j && valves[i].tunnels[j] == null) {
                 valves[i].tunnels[j] = infinity
            }
        }
    }

    valveIds.each { k ->
        valveIds.each { i ->
            valveIds.each { j ->
                def dik = valves[i].tunnels[k]
                def dkj = valves[k].tunnels[j]

                if (dik < infinity && dkj < infinity) {
                     def dij = valves[i].tunnels[j]
                     if (dij == null || dij > dik + dkj) {
                         valves[i].tunnels[j] = dik + dkj
                     }
                }
            }
        }
    }

    def openableValves = valves.values().findAll { it.flow > 0 }.collect { it.id }
    def n = openableValves.size()

    def maxTotalPressure = 0

    (0..<(1 << n)).each { i ->
        def playerMask = i
        def elephantMask = ((1 << n) - 1) - i

        def playerMemo = [:]
        def elephantMemo = [:]

        def playerRelief = maxPressure(valves, "AA", 26, playerMask, openableValves, playerMemo)
        def elephantRelief = maxPressure(valves, "AA", 26, elephantMask, openableValves, elephantMemo)

        maxTotalPressure = Math.max(maxTotalPressure, playerRelief + elephantRelief)
    }

    println(maxTotalPressure)
}

main()

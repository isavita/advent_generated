class Valve {
    String id
    int flow
    Map<String, Integer> tunnels
}

def valves = [:]
def input = new File("input.txt").text
input.split("\n").each { line ->
    def sp = line.split("; ")
    def v = new Valve(id: sp[0].split(" ")[1], flow: sp[0].split("=")[1].toInteger())
    sp[1] = sp[1].substring(sp[1].indexOf("valve") + 5)
    if (sp[1].startsWith("s")) {
        sp[1] = sp[1].substring(2)
    } else {
        sp[1] = sp[1].substring(1)
    }
    v.tunnels = [(v.id): 0]
    sp[1].split(", ").each { t ->
        v.tunnels[t.trim()] = 1
    }
    valves[v.id] = v
}

valves.each { k, v ->
    valves.each { i, iv ->
        valves.each { j, jv ->
            if (iv.tunnels.containsKey(k) && v.tunnels.containsKey(j)) {
                def dik = iv.tunnels[k]
                def dkj = v.tunnels[j]
                def dij = iv.tunnels[j]
                if (dij == null || dij > dik + dkj) {
                    iv.tunnels[j] = dik + dkj
                }
            }
        }
    }
}

def open = valves.findAll { it.value.flow > 0 }.keySet()

println maxPressure(valves, "AA", 30, 0, open as List, 0)

def maxPressure(valves, curr, minute, pressure, open, d) {
    def max = pressure
    open.each { next ->
        def newopen = open - [next]
        def timeLeft = minute - valves[curr].tunnels[next] - 1
        if (timeLeft > 0) {
            max = Math.max(max, maxPressure(valves, next, timeLeft, timeLeft * valves[next].flow + pressure, newopen, d + 1))
        }
    }
    max
}

def divide(l) {
    if (l == 1) {
        return [[[[]], [0]] as Object[], [[0], []] as Object[]]
    }
    def d = divide(l - 1)
    def r = new Object[d.size() * 2]
    for (int i = 0; i < d.size(); i++) {
        r[2 * i] = [d[i][0] + [l - 1], d[i][1]]
        r[2 * i + 1] = [d[i][0], d[i][1] + [l - 1]]
    }
    r
}
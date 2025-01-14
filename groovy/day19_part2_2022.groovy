
class Blueprint {
    int id
    int oreCost
    int clayOreCost
    int obsidianOreCost
    int obsidianClayCost
    int geodeOreCost
    int geodeObsidianCost
}

class State {
    int ore = 0
    int clay = 0
    int obsidian = 0
    int geode = 0
    int oreRobots = 1
    int clayRobots = 0
    int obsidianRobots = 0
    int geodeRobots = 0
    int timeLeft

    State next(Blueprint b) {
        def next = new State()
        next.ore = ore + oreRobots
        next.clay = clay + clayRobots
        next.obsidian = obsidian + obsidianRobots
        next.geode = geode + geodeRobots
        next.oreRobots = oreRobots
        next.clayRobots = clayRobots
        next.obsidianRobots = obsidianRobots
        next.geodeRobots = geodeRobots
        next.timeLeft = timeLeft - 1
        return next
    }

    State buildOreRobot(Blueprint b) {
        def next = next(b)
        next.ore -= b.oreCost
        next.oreRobots++
        return next
    }

    State buildClayRobot(Blueprint b) {
        def next = next(b)
        next.ore -= b.clayOreCost
        next.clayRobots++
        return next
    }

    State buildObsidianRobot(Blueprint b) {
        def next = next(b)
        next.ore -= b.obsidianOreCost
        next.clay -= b.obsidianClayCost
        next.obsidianRobots++
        return next
    }

    State buildGeodeRobot(Blueprint b) {
        def next = next(b)
        next.ore -= b.geodeOreCost
        next.obsidian -= b.geodeObsidianCost
        next.geodeRobots++
        return next
    }

    @Override
    boolean equals(o) {
        if (this.is(o)) return true
        if (getClass() != o.class) return false
        State state = (State) o
        if (ore != state.ore) return false
        if (clay != state.clay) return false
        if (obsidian != state.obsidian) return false
        if (geode != state.geode) return false
        if (oreRobots != state.oreRobots) return false
        if (clayRobots != state.clayRobots) return false
        if (obsidianRobots != state.obsidianRobots) return false
        if (geodeRobots != state.geodeRobots) return false
        if (timeLeft != state.timeLeft) return false
        return true
    }

    @Override
    int hashCode() {
        int result = ore
        result = 31 * result + clay
        result = 31 * result + obsidian
        result = 31 * result + geode
        result = 31 * result + oreRobots
        result = 31 * result + clayRobots
        result = 31 * result + obsidianRobots
        result = 31 * result + geodeRobots
        result = 31 * result + timeLeft
        return result
    }
}

int maxGeode(Blueprint b, State init) {
    int max = 0
    Queue<State> q = new LinkedList<>()
    q.add(init)
    Set<State> visited = new HashSet<>()

    while (!q.isEmpty()) {
        State s = q.poll()
        max = Math.max(max, s.geode)
        if (s.timeLeft == 0) continue

        int maxOreCost = Math.max(Math.max(b.oreCost, b.clayOreCost), Math.max(b.obsidianOreCost, b.geodeOreCost))
        if (s.oreRobots >= maxOreCost) s.oreRobots = maxOreCost
        if (s.clayRobots >= b.obsidianClayCost) s.clayRobots = b.obsidianClayCost
        if (s.obsidianRobots >= b.geodeObsidianCost) s.obsidianRobots = b.geodeObsidianCost

        int maxOre = s.timeLeft * maxOreCost - s.oreRobots * (s.timeLeft - 1)
        if (s.ore >= maxOre) s.ore = maxOre
        int maxClay = s.timeLeft * b.obsidianClayCost - s.clayRobots * (s.timeLeft - 1)
        if (s.clay >= maxClay) s.clay = maxClay
        int maxObsidian = s.timeLeft * b.geodeObsidianCost - s.obsidianRobots * (s.timeLeft - 1)
        if (s.obsidian >= maxObsidian) s.obsidian = maxObsidian

        if (visited.contains(s)) continue
        visited.add(s)

        q.add(s.next(b))

        if (s.ore >= b.oreCost) q.add(s.buildOreRobot(b))
        if (s.ore >= b.clayOreCost) q.add(s.buildClayRobot(b))
        if (s.ore >= b.obsidianOreCost && s.clay >= b.obsidianClayCost) q.add(s.buildObsidianRobot(b))
        if (s.ore >= b.geodeOreCost && s.obsidian >= b.geodeObsidianCost) q.add(s.buildGeodeRobot(b))
    }
    return max
}

def parseBlueprints(File file) {
    file.readLines().collect { line ->
        def parts = line.split(/[:\.]/)
        def nums = parts.collect { it.replaceAll(/[^0-9]/, ' ').trim().split().findAll { it } }.flatten()*.toInteger()
        new Blueprint(
            id: nums[0],
            oreCost: nums[1],
            clayOreCost: nums[2],
            obsidianOreCost: nums[3],
            obsidianClayCost: nums[4],
            geodeOreCost: nums[5],
            geodeObsidianCost: nums[6]
        )
    }
}

def solve() {
    def blueprints = parseBlueprints(new File("input.txt"))
    def init = new State(timeLeft: 32)
    def prod = blueprints.take(3).inject(1) { acc, b -> acc * maxGeode(b, init) }
    println prod
}

solve()


def input = new File("input.txt").text.trim()

class Blueprint {
    int id, oreForOreRobot, oreForClayRobot, oreForObsidianRobot, clayForObsidianRobot, oreForGeodeRobot, obsidianForGeodeRobot
}

class State {
    Blueprint blueprint
    int ore, clay, obsidian, geode, oreRobots, clayRobots, obsidianRobots, geodeRobots

    State(Blueprint blueprint) {
        this.blueprint = blueprint
        oreRobots = 1
    }

    def farm() {
        ore += oreRobots
        clay += clayRobots
        obsidian += obsidianRobots
        geode += geodeRobots
    }

    String hash(int time) {
        "$time,$ore,$clay,$obsidian,$geode,$oreRobots,$clayRobots,$obsidianRobots,$geodeRobots"
    }

    State copy() {
        State copy = new State(blueprint)
        copy.ore = ore
        copy.clay = clay
        copy.obsidian = obsidian
        copy.geode = geode
        copy.oreRobots = oreRobots
        copy.clayRobots = clayRobots
        copy.obsidianRobots = obsidianRobots
        copy.geodeRobots = geodeRobots
        return copy
    }

    int calcMostGeodes(int time, Map<String, Integer> memo, int totalTime, int earliestGeode) {
        if (time == totalTime) {
            return geode
        }

        def h = hash(time)
        if (memo.containsKey(h)) {
            return memo[h]
        }

        if (geode == 0 && time > earliestGeode) {
            return 0
        }

        int mostGeodes = geode

        if (ore >= blueprint.oreForGeodeRobot && obsidian >= blueprint.obsidianForGeodeRobot) {
            def cp = copy()
            cp.farm()
            cp.ore -= cp.blueprint.oreForGeodeRobot
            cp.obsidian -= cp.blueprint.obsidianForGeodeRobot
            cp.geodeRobots++
            if (cp.geodeRobots == 1) {
                earliestGeode = Math.min(earliestGeode, time + 1)
            }
            mostGeodes = Math.max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode))
            memo[h] = mostGeodes
            return mostGeodes
        }

        if (time <= totalTime - 16 && oreRobots < blueprint.oreForObsidianRobot * 2 && ore >= blueprint.oreForOreRobot) {
            def cp = copy()
            cp.ore -= cp.blueprint.oreForOreRobot
            cp.farm()
            cp.oreRobots++
            mostGeodes = Math.max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode))
        }

        if (time <= totalTime - 8 && clayRobots < blueprint.clayForObsidianRobot && ore >= blueprint.oreForClayRobot) {
            def cp = copy()
            cp.ore -= cp.blueprint.oreForClayRobot
            cp.farm()
            cp.clayRobots++
            mostGeodes = Math.max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode))
        }

        if (time <= totalTime - 4 && obsidianRobots < blueprint.obsidianForGeodeRobot && ore >= blueprint.oreForObsidianRobot && clay >= blueprint.clayForObsidianRobot) {
            def cp = copy()
            cp.ore -= cp.blueprint.oreForObsidianRobot
            cp.clay -= cp.blueprint.clayForObsidianRobot
            cp.farm()
            cp.obsidianRobots++
            mostGeodes = Math.max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode))
        }

        def cp = copy()
        cp.farm()
        mostGeodes = Math.max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode))

        memo[h] = mostGeodes
        return mostGeodes
    }
}

def parseInput(String input) {
    def ans = []
    input.eachLine { line ->
        def bp = new Blueprint()
        def matcher = line =~ /Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian./
        if (matcher.matches()) {
            bp.id = matcher[0][1].toInteger()
            bp.oreForOreRobot = matcher[0][2].toInteger()
            bp.oreForClayRobot = matcher[0][3].toInteger()
            bp.oreForObsidianRobot = matcher[0][4].toInteger()
            bp.clayForObsidianRobot = matcher[0][5].toInteger()
            bp.oreForGeodeRobot = matcher[0][6].toInteger()
            bp.obsidianForGeodeRobot = matcher[0][7].toInteger()
            ans << bp
        }
    }
    ans
}

def part1(String input) {
    def blueprints = parseInput(input)
    def sum = 0
    blueprints.each { bp ->
        def st = new State(bp)
        def geodesMade = st.calcMostGeodes(0, [:], 24, 24)
        sum += st.blueprint.id * geodesMade
    }
    sum
}

println part1(input)

const fs = require('fs');

class Blueprint {
    constructor(id, oreCost, clayOreCost, obsidianOreCost, obsidianClayCost, geodeOreCost, geodeObsidianCost) {
        this.id = id;
        this.oreCost = oreCost;
        this.clay = { oreCost: clayOreCost };
        this.obsidian = { oreCost: obsidianOreCost, clayCost: obsidianClayCost };
        this.geode = { oreCost: geodeOreCost, obsidianCost: geodeObsidianCost };
    }
}

class State {
    constructor(ore = 0, clay = 0, obsidian = 0, geode = 0, oreRobots = 1, clayRobots = 0, obsidianRobots = 0, geodeRobots = 0, timeLeft = 24) {
        this.ore = ore;
        this.clay = clay;
        this.obsidian = obsidian;
        this.geode = geode;
        this.oreRobots = oreRobots;
        this.clayRobots = clayRobots;
        this.obsidianRobots = obsidianRobots;
        this.geodeRobots = geodeRobots;
        this.timeLeft = timeLeft;
    }

    toString() {
        return `${this.ore},${this.clay},${this.obsidian},${this.geode},${this.oreRobots},${this.clayRobots},${this.obsidianRobots},${this.geodeRobots},${this.timeLeft}`;
    }
}

function maxGeode(blueprint, initialState) {
    let max = 0;
    let queue = [initialState];
    let visited = new Set();

    while (queue.length > 0) {
        queue.sort((a, b) => b.geode - a.geode || b.obsidian - a.obsidian || b.clay - a.clay || b.ore - a.ore);
        let state = queue.shift();
        max = Math.max(max, state.geode);

        if (state.timeLeft === 0) continue;

        let maxOreNeeded = Math.max(blueprint.oreCost, blueprint.clay.oreCost, blueprint.obsidian.oreCost, blueprint.geode.oreCost);
        if (state.oreRobots >= maxOreNeeded) state.oreRobots = maxOreNeeded;
        if (state.clayRobots >= blueprint.obsidian.clayCost) state.clayRobots = blueprint.obsidian.clayCost;
        if (state.obsidianRobots >= blueprint.geode.obsidianCost) state.obsidianRobots = blueprint.geode.obsidianCost;

        let maxOre = state.timeLeft * maxOreNeeded - state.oreRobots * (state.timeLeft - 1);
        if (state.ore >= maxOre) state.ore = maxOre;
        let maxClay = state.timeLeft * blueprint.obsidian.clayCost - state.clayRobots * (state.timeLeft - 1);
        if (state.clay >= maxClay) state.clay = maxClay;
        let maxObsidian = state.timeLeft * blueprint.geode.obsidianCost - state.obsidianRobots * (state.timeLeft - 1);
        if (state.obsidian >= maxObsidian) state.obsidian = maxObsidian;

        let stateKey = state.toString();
        if (visited.has(stateKey)) continue;
        visited.add(stateKey);

        queue.push(new State(
            state.ore + state.oreRobots,
            state.clay + state.clayRobots,
            state.obsidian + state.obsidianRobots,
            state.geode + state.geodeRobots,
            state.oreRobots,
            state.clayRobots,
            state.obsidianRobots,
            state.geodeRobots,
            state.timeLeft - 1
        ));

        if (state.ore >= blueprint.oreCost) {
            queue.push(new State(
                state.ore - blueprint.oreCost + state.oreRobots,
                state.clay + state.clayRobots,
                state.obsidian + state.obsidianRobots,
                state.geode + state.geodeRobots,
                state.oreRobots + 1,
                state.clayRobots,
                state.obsidianRobots,
                state.geodeRobots,
                state.timeLeft - 1
            ));
        }

        if (state.ore >= blueprint.clay.oreCost) {
            queue.push(new State(
                state.ore - blueprint.clay.oreCost + state.oreRobots,
                state.clay + state.clayRobots,
                state.obsidian + state.obsidianRobots,
                state.geode + state.geodeRobots,
                state.oreRobots,
                state.clayRobots + 1,
                state.obsidianRobots,
                state.geodeRobots,
                state.timeLeft - 1
            ));
        }

        if (state.ore >= blueprint.obsidian.oreCost && state.clay >= blueprint.obsidian.clayCost) {
            queue.push(new State(
                state.ore - blueprint.obsidian.oreCost + state.oreRobots,
                state.clay - blueprint.obsidian.clayCost + state.clayRobots,
                state.obsidian + state.obsidianRobots,
                state.geode + state.geodeRobots,
                state.oreRobots,
                state.clayRobots,
                state.obsidianRobots + 1,
                state.geodeRobots,
                state.timeLeft - 1
            ));
        }

        if (state.ore >= blueprint.geode.oreCost && state.obsidian >= blueprint.geode.obsidianCost) {
            queue.push(new State(
                state.ore - blueprint.geode.oreCost + state.oreRobots,
                state.clay + state.clayRobots,
                state.obsidian - blueprint.geode.obsidianCost + state.obsidianRobots,
                state.geode + state.geodeRobots,
                state.oreRobots,
                state.clayRobots,
                state.obsidianRobots,
                state.geodeRobots + 1,
                state.timeLeft - 1
            ));
        }
    }
    return max;
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8').split('\n');
    const blueprints = [];

    for (let line of input) {
        const [id, oreCost, clayOreCost, obsidianOreCost, obsidianClayCost, geodeOreCost, geodeObsidianCost] = line.match(/\d+/g).map(Number);
        blueprints.push(new Blueprint(id, oreCost, clayOreCost, obsidianOreCost, obsidianClayCost, geodeOreCost, geodeObsidianCost));
    }

    let initialState = new State();
    initialState.timeLeft = 32;

    let product = 1;
    for (let i = 0; i < 3; i++) {
        product *= maxGeode(blueprints[i], initialState);
    }

    console.log(product);
}

main();
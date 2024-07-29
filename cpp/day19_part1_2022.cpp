#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <unordered_map>
#include <algorithm>

struct Blueprint {
    int id, oreForOreRobot, oreForClayRobot, oreForObsidianRobot, clayForObsidianRobot;
    int oreForGeodeRobot, obsidianForGeodeRobot;
};

struct State {
    Blueprint blueprint;
    int ore, clay, obsidian, geode;
    int oreRobots, clayRobots, obsidianRobots, geodeRobots;

    State(Blueprint bp) : blueprint(bp), ore(0), clay(0), obsidian(0), geode(0), oreRobots(1), clayRobots(0), obsidianRobots(0), geodeRobots(0) {}

    void farm() {
        ore += oreRobots;
        clay += clayRobots;
        obsidian += obsidianRobots;
        geode += geodeRobots;
    }

    std::string hash(int time) const {
        return std::to_string(time) + "," + std::to_string(ore) + "," + std::to_string(clay) + "," +
               std::to_string(obsidian) + "," + std::to_string(geode) + "," +
               std::to_string(oreRobots) + "," + std::to_string(clayRobots) + "," +
               std::to_string(obsidianRobots) + "," + std::to_string(geodeRobots);
    }

    State copy() const {
        return *this;
    }

    int calcMostGeodes(int time, std::unordered_map<std::string, int>& memo, int totalTime, int earliestGeode) {
        if (time == totalTime) return geode;

        std::string h = hash(time);
        if (memo.count(h)) return memo[h];

        if (geode == 0 && time > earliestGeode) return 0;

        int mostGeodes = geode;

        if (ore >= blueprint.oreForGeodeRobot && obsidian >= blueprint.obsidianForGeodeRobot) {
            State cp = copy();
            cp.farm();
            cp.ore -= blueprint.oreForGeodeRobot;
            cp.obsidian -= blueprint.obsidianForGeodeRobot;
            cp.geodeRobots++;
            if (cp.geodeRobots == 1) earliestGeode = std::min(earliestGeode, time + 1);
            mostGeodes = std::max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode));
        }

        if (time <= totalTime - 16 && oreRobots < blueprint.oreForObsidianRobot * 2 && ore >= blueprint.oreForOreRobot) {
            State cp = copy();
            cp.ore -= blueprint.oreForOreRobot;
            cp.farm();
            cp.oreRobots++;
            mostGeodes = std::max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode));
        }

        if (time <= totalTime - 8 && clayRobots < blueprint.clayForObsidianRobot && ore >= blueprint.oreForClayRobot) {
            State cp = copy();
            cp.ore -= blueprint.oreForClayRobot;
            cp.farm();
            cp.clayRobots++;
            mostGeodes = std::max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode));
        }

        if (time <= totalTime - 4 && obsidianRobots < blueprint.obsidianForGeodeRobot && ore >= blueprint.oreForObsidianRobot && clay >= blueprint.clayForObsidianRobot) {
            State cp = copy();
            cp.ore -= blueprint.oreForObsidianRobot;
            cp.clay -= blueprint.clayForObsidianRobot;
            cp.farm();
            cp.obsidianRobots++;
            mostGeodes = std::max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode));
        }

        State cp = copy();
        cp.farm();
        mostGeodes = std::max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode));

        memo[h] = mostGeodes;
        return mostGeodes;
    }
};

std::vector<Blueprint> parseInput(const std::string& input) {
    std::vector<Blueprint> blueprints;
    std::istringstream iss(input);
    std::string line;
    while (std::getline(iss, line)) {
        Blueprint bp;
        sscanf(line.c_str(), "Blueprint %d: Each ore robot costs %d ore. Each clay robot costs %d ore. Each obsidian robot costs %d ore and %d clay. Each geode robot costs %d ore and %d obsidian.",
               &bp.id, &bp.oreForOreRobot, &bp.oreForClayRobot, &bp.oreForObsidianRobot,
               &bp.clayForObsidianRobot, &bp.oreForGeodeRobot, &bp.obsidianForGeodeRobot);
        blueprints.push_back(bp);
    }
    return blueprints;
}

int main() {
    std::ifstream file("input.txt");
    std::string input((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    input.erase(std::remove(input.begin(), input.end(), '\r'), input.end());

    std::vector<Blueprint> blueprints = parseInput(input);
    int sum = 0;

    for (const auto& bp : blueprints) {
        State st(bp);
        std::unordered_map<std::string, int> memo;
        int geodesMade = st.calcMostGeodes(0, memo, 24, 24);
        sum += st.blueprint.id * geodesMade;
    }

    std::cout << sum << std::endl;
    return 0;
}
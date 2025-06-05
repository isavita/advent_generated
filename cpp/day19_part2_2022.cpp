
#include <iostream>
#include <fstream>
#include <string>
#include <deque>
#include <unordered_set>
#include <algorithm> // For std::max, std::min
#include <vector>
#include <cstdio>    // For sscanf

struct Blueprint {
    int id;
    int ore_cost;
    int clay_ore_cost;
    int obsidian_ore_cost;
    int obsidian_clay_cost;
    int geode_ore_cost;
    int geode_obsidian_cost;
};

struct State {
    int ore, clay, obsidian, geode;
    int ore_robots, clay_robots, obsidian_robots, geode_robots;
    int time_left;

    State(int o = 0, int c = 0, int ob = 0, int g = 0, int orob = 0, int clrob = 0, int obrob = 0, int grob = 0, int tl = 0)
        : ore(o), clay(c), obsidian(ob), geode(g),
          ore_robots(orob), clay_robots(clrob), obsidian_robots(obrob), geode_robots(grob),
          time_left(tl) {}

    bool operator==(const State& other) const {
        return ore == other.ore && clay == other.clay && obsidian == other.obsidian && geode == other.geode &&
               ore_robots == other.ore_robots && clay_robots == other.clay_robots &&
               obsidian_robots == other.obsidian_robots && geode_robots == other.geode_robots &&
               time_left == other.time_left;
    }
};

struct StateHash {
    size_t operator()(const State& s) const {
        size_t h = 0;
        auto hash_combine = [&](size_t& seed, int v) {
            seed ^= std::hash<int>()(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
        };

        hash_combine(h, s.ore);
        hash_combine(h, s.clay);
        hash_combine(h, s.obsidian);
        hash_combine(h, s.geode);
        hash_combine(h, s.ore_robots);
        hash_combine(h, s.clay_robots);
        hash_combine(h, s.obsidian_robots);
        hash_combine(h, s.geode_robots);
        hash_combine(h, s.time_left);
        return h;
    }
};

int maxGeode(const Blueprint& b, State initial_state) {
    int max_geodes = 0;
    std::deque<State> q;
    q.push_back(initial_state);

    std::unordered_set<State, StateHash> visited;

    while (!q.empty()) {
        State s = q.front();
        q.pop_front();

        max_geodes = std::max(max_geodes, s.geode);

        if (s.time_left == 0) {
            continue;
        }

        // Pruning logic for robots
        int max_ore_cost = std::max({b.ore_cost, b.clay_ore_cost, b.obsidian_ore_cost, b.geode_ore_cost});
        s.ore_robots = std::min(s.ore_robots, max_ore_cost);
        s.clay_robots = std::min(s.clay_robots, b.obsidian_clay_cost);
        s.obsidian_robots = std::min(s.obsidian_robots, b.geode_obsidian_cost);

        // Pruning logic for resources
        // If current resource + future production exceeds what's maximally needed, cap it.
        s.ore = std::min(s.ore, s.time_left * max_ore_cost - s.ore_robots * (s.time_left - 1));
        s.clay = std::min(s.clay, s.time_left * b.obsidian_clay_cost - s.clay_robots * (s.time_left - 1));
        s.obsidian = std::min(s.obsidian, s.time_left * b.geode_obsidian_cost - s.obsidian_robots * (s.time_left - 1));

        if (!visited.insert(s).second) {
            continue;
        }

        // Option 1: Do nothing, just collect resources
        q.push_back(State(s.ore + s.ore_robots,
                          s.clay + s.clay_robots,
                          s.obsidian + s.obsidian_robots,
                          s.geode + s.geode_robots,
                          s.ore_robots, s.clay_robots, s.obsidian_robots, s.geode_robots,
                          s.time_left - 1));

        // Option 2: Try to build an ore robot
        if (s.ore >= b.ore_cost) {
            q.push_back(State(s.ore - b.ore_cost + s.ore_robots,
                              s.clay + s.clay_robots,
                              s.obsidian + s.obsidian_robots,
                              s.geode + s.geode_robots,
                              s.ore_robots + 1, s.clay_robots, s.obsidian_robots, s.geode_robots,
                              s.time_left - 1));
        }

        // Option 3: Try to build a clay robot
        if (s.ore >= b.clay_ore_cost) {
            q.push_back(State(s.ore - b.clay_ore_cost + s.ore_robots,
                              s.clay + s.clay_robots,
                              s.obsidian + s.obsidian_robots,
                              s.geode + s.geode_robots,
                              s.ore_robots, s.clay_robots + 1, s.obsidian_robots, s.geode_robots,
                              s.time_left - 1));
        }

        // Option 4: Try to build an obsidian robot
        if (s.ore >= b.obsidian_ore_cost && s.clay >= b.obsidian_clay_cost) {
            q.push_back(State(s.ore - b.obsidian_ore_cost + s.ore_robots,
                              s.clay - b.obsidian_clay_cost + s.clay_robots,
                              s.obsidian + s.obsidian_robots,
                              s.geode + s.geode_robots,
                              s.ore_robots, s.clay_robots, s.obsidian_robots + 1, s.geode_robots,
                              s.time_left - 1));
        }

        // Option 5: Try to build a geode robot
        if (s.ore >= b.geode_ore_cost && s.obsidian >= b.geode_obsidian_cost) {
            q.push_back(State(s.ore - b.geode_ore_cost + s.ore_robots,
                              s.clay + s.clay_robots,
                              s.obsidian - b.geode_obsidian_cost + s.obsidian_robots,
                              s.geode + s.geode_robots,
                              s.ore_robots, s.clay_robots, s.obsidian_robots, s.geode_robots + 1,
                              s.time_left - 1));
        }
    }
    return max_geodes;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<Blueprint> blueprints;
    std::ifstream file("input.txt");
    std::string line;

    if (!file.is_open()) {
        return 1;
    }

    while (std::getline(file, line)) {
        Blueprint b;
        sscanf(line.c_str(), "Blueprint %d: Each ore robot costs %d ore. Each clay robot costs %d ore. Each obsidian robot costs %d ore and %d clay. Each geode robot costs %d ore and %d obsidian.",
               &b.id, &b.ore_cost, &b.clay_ore_cost,
               &b.obsidian_ore_cost, &b.obsidian_clay_cost,
               &b.geode_ore_cost, &b.geode_obsidian_cost);
        blueprints.push_back(b);
    }
    file.close();

    State initial_state(0, 0, 0, 0, 1, 0, 0, 0, 32);
    long long product_of_max_geodes = 1;

    for (size_t i = 0; i < std::min((size_t)3, blueprints.size()); ++i) {
        product_of_max_geodes *= maxGeode(blueprints[i], initial_state);
    }

    std::cout << product_of_max_geodes << std::endl;

    return 0;
}

#include <iostream>
#include <fstream>
#include <vector>
#include <regex>

struct Nanobot {
    int X, Y, Z, Radius;
};

std::vector<Nanobot> parseNanobots(std::ifstream& file);
Nanobot findStrongestNanobot(const std::vector<Nanobot>& nanobots);
int countNanobotsInRange(const std::vector<Nanobot>& nanobots, const Nanobot& strongest);
int manhattanDistance(const Nanobot& a, const Nanobot& b);
int abs(int x);

int main() {
    std::ifstream file("input.txt");
    std::vector<Nanobot> nanobots = parseNanobots(file);

    Nanobot strongest = findStrongestNanobot(nanobots);
    int inRangeCount = countNanobotsInRange(nanobots, strongest);

    std::cout << inRangeCount << std::endl;

    return 0;
}

std::vector<Nanobot> parseNanobots(std::ifstream& file) {
    std::vector<Nanobot> nanobots;
    std::regex re("pos=<(-?\\d+),(-?\\d+),(-?\\d+)>, r=(\\d+)");
    std::string line;

    while (std::getline(file, line)) {
        std::smatch matches;
        if (std::regex_search(line, matches, re)) {
            int x = std::stoi(matches[1]);
            int y = std::stoi(matches[2]);
            int z = std::stoi(matches[3]);
            int radius = std::stoi(matches[4]);

            nanobots.push_back(Nanobot{x, y, z, radius});
        }
    }

    return nanobots;
}

Nanobot findStrongestNanobot(const std::vector<Nanobot>& nanobots) {
    Nanobot strongest;
    for (const Nanobot& nanobot : nanobots) {
        if (nanobot.Radius > strongest.Radius) {
            strongest = nanobot;
        }
    }
    return strongest;
}

int countNanobotsInRange(const std::vector<Nanobot>& nanobots, const Nanobot& strongest) {
    int count = 0;
    for (const Nanobot& nanobot : nanobots) {
        if (manhattanDistance(nanobot, strongest) <= strongest.Radius) {
            count++;
        }
    }
    return count;
}

int manhattanDistance(const Nanobot& a, const Nanobot& b) {
    return abs(a.X - b.X) + abs(a.Y - b.Y) + abs(a.Z - b.Z);
}

int abs(int x) {
    if (x < 0) {
        return -x;
    }
    return x;
}
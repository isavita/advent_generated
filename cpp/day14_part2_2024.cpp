
#include <fstream>
#include <iostream>
#include <regex>
#include <set>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

struct Robot {
    int x, y, vx, vy;
};

int mod(int a, int b) {
    return (a % b + b) % b;
}

Robot parseLine(const std::string& line) {
    std::regex pattern(R"(p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+))");
    std::smatch matches;
    if (std::regex_match(line, matches, pattern)) {
        return {std::stoi(matches[1]), std::stoi(matches[2]),
                std::stoi(matches[3]), std::stoi(matches[4])};
    }
    throw std::runtime_error("Invalid line format: " + line);
}

void moveRobots(std::vector<Robot>& robots, int size_x, int size_y) {
    for (auto& r : robots) {
        r.x = mod(r.x + r.vx, size_x);
        r.y = mod(r.y + r.vy, size_y);
    }
}

std::vector<long long> countQuadrants(const std::vector<Robot>& robots, int size_x, int size_y) {
    std::vector<long long> counts(4, 0);
    int center_x = size_x / 2;
    int center_y = size_y / 2;
    for (const auto& r : robots) {
        if (r.x < center_x) {
            if (r.y < center_y) {
                counts[0]++;
            } else if (r.y > center_y) {
                counts[1]++;
            }
        } else if (r.x > center_x) {
            if (r.y < center_y) {
                counts[2]++;
            } else if (r.y > center_y) {
                counts[3]++;
            }
        }
    }
    return counts;
}

bool hasNoOverlaps(const std::vector<Robot>& robots) {
    std::set<std::pair<int, int>> positions;
    for (const auto& r : robots) {
        if (!positions.insert({r.x, r.y}).second) {
            return false;
        }
    }
    return true;
}

void drawGrid(const std::vector<Robot>& robots, int size_x, int size_y) {
    std::vector<std::vector<char>> grid(size_y, std::vector<char>(size_x, '.'));
    for (const auto& r : robots) {
        grid[r.y][r.x] = '#';
    }
    for (int y = 0; y < size_y; ++y) {
        for (int x = 0; x < size_x; ++x) {
            std::cout << grid[y][x];
        }
        std::cout << '\n';
    }
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    const int size_x = 101;
    const int size_y = 103;
    std::vector<Robot> robots;

    std::ifstream inputFile("input.txt");
    std::string line;
    while (std::getline(inputFile, line)) {
        if (!line.empty()) {
            robots.push_back(parseLine(line));
        }
    }
    inputFile.close();

    std::vector<Robot> robots_part1 = robots;
    for (int i = 0; i < 100; ++i) {
        moveRobots(robots_part1, size_x, size_y);
    }
    std::vector<long long> counts = countQuadrants(robots_part1, size_x, size_y);
    long long safety_factor = 1;
    for (long long c : counts) {
        safety_factor *= c;
    }
    std::cout << "Part 1 - Safety Factor after 100 seconds: " << safety_factor << '\n';

    std::vector<Robot> robots_part2 = robots;
    long long seconds = 0;
    const long long max_iterations = 1000000;

    while (true) {
        if (hasNoOverlaps(robots_part2)) {
            break;
        }
        moveRobots(robots_part2, size_x, size_y);
        seconds++;
        if (seconds > max_iterations) {
            std::cout << "Exceeded maximum iterations without finding a unique position configuration.\n";
            return 1;
        }
    }
    std::cout << "Part 2 - Fewest seconds to display Easter egg: " << seconds << '\n';
    std::cout << "Final positions of robots:\n";
    drawGrid(robots_part2, size_x, size_y);

    return 0;
}

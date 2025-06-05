
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <regex>
#include <numeric>
#include <cmath>

struct Vec3 {
    int x, y, z;
};

long long lcm(long long a, long long b) {
    if (a == 0 || b == 0) return 0;
    return std::abs(a * b) / std::gcd(a, b);
}

void parseInput(const std::string& filename, std::vector<Vec3>& positions, std::vector<Vec3>& velocities) {
    std::ifstream file(filename);
    std::string line;
    std::regex num_regex("-?\\d+");

    while (std::getline(file, line)) {
        std::sregex_iterator it(line.begin(), line.end(), num_regex);

        int x = std::stoi(it->str());
        ++it;
        int y = std::stoi(it->str());
        ++it;
        int z = std::stoi(it->str());

        positions.push_back({x, y, z});
        velocities.push_back({0, 0, 0});
    }
}

void simulateMotion(std::vector<Vec3>& positions, std::vector<Vec3>& velocities, int steps) {
    for (int s = 0; s < steps; ++s) {
        for (size_t i = 0; i < positions.size(); ++i) {
            for (size_t j = i + 1; j < positions.size(); ++j) {
                if (positions[i].x < positions[j].x) {
                    velocities[i].x++;
                    velocities[j].x--;
                } else if (positions[i].x > positions[j].x) {
                    velocities[i].x--;
                    velocities[j].x++;
                }
                if (positions[i].y < positions[j].y) {
                    velocities[i].y++;
                    velocities[j].y--;
                } else if (positions[i].y > positions[j].y) {
                    velocities[i].y--;
                    velocities[j].y++;
                }
                if (positions[i].z < positions[j].z) {
                    velocities[i].z++;
                    velocities[j].z--;
                } else if (positions[i].z > positions[j].z) {
                    velocities[i].z--;
                    velocities[j].z++;
                }
            }
        }

        for (size_t i = 0; i < positions.size(); ++i) {
            positions[i].x += velocities[i].x;
            positions[i].y += velocities[i].y;
            positions[i].z += velocities[i].z;
        }
    }
}

long long calculateEnergy(const std::vector<Vec3>& positions, const std::vector<Vec3>& velocities) {
    long long totalEnergy = 0;
    for (size_t i = 0; i < positions.size(); ++i) {
        long long potentialEnergy = std::abs(positions[i].x) + std::abs(positions[i].y) + std::abs(positions[i].z);
        long long kineticEnergy = std::abs(velocities[i].x) + std::abs(velocities[i].y) + std::abs(velocities[i].z);
        totalEnergy += potentialEnergy * kineticEnergy;
    }
    return totalEnergy;
}

long long findCycleLength(std::vector<Vec3> positions, std::vector<Vec3> velocities) {
    std::vector<long long> cycleLengths(3, 0);

    std::vector<std::vector<std::pair<int, int>>> initialStates(3);
    for (size_t i = 0; i < positions.size(); ++i) {
        initialStates[0].push_back({positions[i].x, velocities[i].x});
        initialStates[1].push_back({positions[i].y, velocities[i].y});
        initialStates[2].push_back({positions[i].z, velocities[i].z});
    }

    for (int axis = 0; axis < 3; ++axis) {
        long long steps = 0;
        while (true) {
            for (size_t i = 0; i < positions.size(); ++i) {
                for (size_t j = i + 1; j < positions.size(); ++j) {
                    if (axis == 0) {
                        if (positions[i].x < positions[j].x) {
                            velocities[i].x++;
                            velocities[j].x--;
                        } else if (positions[i].x > positions[j].x) {
                            velocities[i].x--;
                            velocities[j].x++;
                        }
                    } else if (axis == 1) {
                        if (positions[i].y < positions[j].y) {
                            velocities[i].y++;
                            velocities[j].y--;
                        } else if (positions[i].y > positions[j].y) {
                            velocities[i].y--;
                            velocities[j].y++;
                        }
                    } else {
                        if (positions[i].z < positions[j].z) {
                            velocities[i].z++;
                            velocities[j].z--;
                        } else if (positions[i].z > positions[j].z) {
                            velocities[i].z--;
                            velocities[j].z++;
                        }
                    }
                }
            }

            for (size_t i = 0; i < positions.size(); ++i) {
                if (axis == 0) positions[i].x += velocities[i].x;
                else if (axis == 1) positions[i].y += velocities[i].y;
                else positions[i].z += velocities[i].z;
            }
            steps++;

            bool match = true;
            for (size_t i = 0; i < positions.size(); ++i) {
                if (axis == 0) {
                    if (positions[i].x != initialStates[0][i].first || velocities[i].x != initialStates[0][i].second) {
                        match = false;
                        break;
                    }
                } else if (axis == 1) {
                    if (positions[i].y != initialStates[1][i].first || velocities[i].y != initialStates[1][i].second) {
                        match = false;
                        break;
                    }
                } else {
                    if (positions[i].z != initialStates[2][i].first || velocities[i].z != initialStates[2][i].second) {
                        match = false;
                        break;
                    }
                }
            }

            if (match) {
                cycleLengths[axis] = steps;
                break;
            }
        }
    }

    return lcm(cycleLengths[0], lcm(cycleLengths[1], cycleLengths[2]));
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<Vec3> positions;
    std::vector<Vec3> velocities;
    parseInput("input.txt", positions, velocities);

    std::vector<Vec3> positionsP1 = positions;
    std::vector<Vec3> velocitiesP1 = velocities;
    simulateMotion(positionsP1, velocitiesP1, 1000);
    long long totalEnergy = calculateEnergy(positionsP1, velocitiesP1);
    std::cout << "Part 1 - Total energy after 1000 steps: " << totalEnergy << std::endl;

    long long cycleLength = findCycleLength(positions, velocities); 
    std::cout << "Part 2 - Number of steps to reach the first repeating state: " << cycleLength << std::endl;

    return 0;
}


#include <iostream>
#include <fstream>
#include <vector>
#include <sstream>
#include <cmath>

struct Moon {
    int x, y, z;
    int vx, vy, vz;
};

void applyGravity(std::vector<Moon>& moons) {
    int n = moons.size();
    for (int i = 0; i < n; ++i) {
        for (int j = i + 1; j < n; ++j) {
            if (moons[i].x < moons[j].x) {
                moons[i].vx += 1;
                moons[j].vx -= 1;
            } else if (moons[i].x > moons[j].x) {
                moons[i].vx -= 1;
                moons[j].vx += 1;
            }

            if (moons[i].y < moons[j].y) {
                moons[i].vy += 1;
                moons[j].vy -= 1;
            } else if (moons[i].y > moons[j].y) {
                moons[i].vy -= 1;
                moons[j].vy += 1;
            }

            if (moons[i].z < moons[j].z) {
                moons[i].vz += 1;
                moons[j].vz -= 1;
            } else if (moons[i].z > moons[j].z) {
                moons[i].vz -= 1;
                moons[j].vz += 1;
            }
        }
    }
}

void applyVelocity(std::vector<Moon>& moons) {
    for (auto& moon : moons) {
        moon.x += moon.vx;
        moon.y += moon.vy;
        moon.z += moon.vz;
    }
}

int calculateTotalEnergy(const std::vector<Moon>& moons) {
    int totalEnergy = 0;
    for (const auto& moon : moons) {
        int potentialEnergy = std::abs(moon.x) + std::abs(moon.y) + std::abs(moon.z);
        int kineticEnergy = std::abs(moon.vx) + std::abs(moon.vy) + std::abs(moon.vz);
        totalEnergy += potentialEnergy * kineticEnergy;
    }
    return totalEnergy;
}

int main() {
    std::ifstream inputFile("input.txt");
    if (!inputFile) {
        std::cerr << "Error opening input file." << std::endl;
        return 1;
    }

    std::vector<Moon> moons;
    std::string line;
    while (std::getline(inputFile, line)) {
        Moon moon;
        std::istringstream iss(line);
        char dummy;
        iss >> dummy >> dummy >> dummy >> moon.x >> dummy >> dummy >> dummy >> moon.y >> dummy >> dummy >> dummy >> moon.z >> dummy;
        moon.vx = moon.vy = moon.vz = 0;
        moons.push_back(moon);
    }
    inputFile.close();

    const int steps = 1000;
    for (int step = 0; step < steps; ++step) {
        applyGravity(moons);
        applyVelocity(moons);
    }

    int totalEnergy = calculateTotalEnergy(moons);
    std::cout << "Total energy in the system after " << steps << " steps: " << totalEnergy << std::endl;

    return 0;
}

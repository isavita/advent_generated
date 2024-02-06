
#include <iostream>
#include <fstream>
#include <vector>
#include <sstream>

struct Reindeer {
    int speed;
    int flyTime;
    int restTime;
    int distance;
    int points;
    bool flying;
    int timeInMode;
};

std::vector<Reindeer> readReindeerDetails(std::string filename) {
    std::ifstream file(filename);
    std::vector<Reindeer> reindeers;
    std::string line;

    while (std::getline(file, line)) {
        std::istringstream iss(line);
        std::string token;
        std::vector<std::string> parts;

        while (iss >> token) {
            parts.push_back(token);
        }

        int speed = std::stoi(parts[3]);
        int flyTime = std::stoi(parts[6]);
        int restTime = std::stoi(parts[13]);

        reindeers.push_back({speed, flyTime, restTime, 0, 0, true, 0});
    }

    return reindeers;
}

void simulateRaceWithPoints(std::vector<Reindeer>& reindeers, int totalSeconds) {
    for (int i = 0; i < totalSeconds; i++) {
        int maxDistance = 0;
        for (int j = 0; j < reindeers.size(); j++) {
            Reindeer& reindeer = reindeers[j];
            if (reindeer.flying) {
                reindeer.distance += reindeer.speed;
            }
            reindeer.timeInMode++;
            if ((reindeer.flying && reindeer.timeInMode == reindeer.flyTime) || (!reindeer.flying && reindeer.timeInMode == reindeer.restTime)) {
                reindeer.flying = !reindeer.flying;
                reindeer.timeInMode = 0;
            }
            if (reindeer.distance > maxDistance) {
                maxDistance = reindeer.distance;
            }
        }
        for (int j = 0; j < reindeers.size(); j++) {
            Reindeer& reindeer = reindeers[j];
            if (reindeer.distance == maxDistance) {
                reindeer.points++;
            }
        }
    }
}

int findMaxPoints(std::vector<Reindeer>& reindeers) {
    int maxPoints = 0;
    for (const auto& reindeer : reindeers) {
        if (reindeer.points > maxPoints) {
            maxPoints = reindeer.points;
        }
    }
    return maxPoints;
}

int main() {
    std::vector<Reindeer> reindeers = readReindeerDetails("input.txt");
    simulateRaceWithPoints(reindeers, 2503);
    int maxPoints = findMaxPoints(reindeers);
    std::cout << maxPoints << std::endl;
    return 0;
}


#include <iostream>
#include <fstream>
#include <vector>
#include <sstream>

struct Reindeer {
    int speed;
    int flyTime;
    int restTime;
    int distance;
    bool flying;
    int timeInMode;
};

std::vector<Reindeer> readReindeerDetails(std::string filename);
void simulateRace(std::vector<Reindeer>& reindeers, int totalSeconds);
int findMaxDistance(std::vector<Reindeer>& reindeers);

int main() {
    std::vector<Reindeer> reindeers = readReindeerDetails("input.txt");
    simulateRace(reindeers, 2503);
    int maxDistance = findMaxDistance(reindeers);
    std::cout << maxDistance << std::endl;
    return 0;
}

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

        reindeers.push_back({speed, flyTime, restTime, 0, true, 0});
    }
    return reindeers;
}

void simulateRace(std::vector<Reindeer>& reindeers, int totalSeconds) {
    for (int i = 0; i < totalSeconds; i++) {
        for (int j = 0; j < reindeers.size(); j++) {
            Reindeer& reindeer = reindeers[j];
            if (reindeer.flying) {
                reindeer.distance += reindeer.speed;
                reindeer.timeInMode++;
                if (reindeer.timeInMode == reindeer.flyTime) {
                    reindeer.flying = false;
                    reindeer.timeInMode = 0;
                }
            } else {
                reindeer.timeInMode++;
                if (reindeer.timeInMode == reindeer.restTime) {
                    reindeer.flying = true;
                    reindeer.timeInMode = 0;
                }
            }
        }
    }
}

int findMaxDistance(std::vector<Reindeer>& reindeers) {
    int maxDistance = 0;
    for (const auto& reindeer : reindeers) {
        if (reindeer.distance > maxDistance) {
            maxDistance = reindeer.distance;
        }
    }
    return maxDistance;
}

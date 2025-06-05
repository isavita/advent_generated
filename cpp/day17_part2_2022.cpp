
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <set>
#include <map>
#include <tuple>
#include <algorithm>

const int CHAMBER_WIDTH = 7;
const int PROFILE_DEPTH = 30;

const std::vector<std::vector<std::pair<int, int>>> rockShapes = {
    {{0,0}, {1,0}, {2,0}, {3,0}},
    {{1,0}, {0,1}, {1,1}, {2,1}, {1,2}},
    {{0,0}, {1,0}, {2,0}, {2,1}, {2,2}},
    {{0,0}, {0,1}, {0,2}, {0,3}},
    {{0,0}, {1,0}, {0,1}, {1,1}}
};

std::string readInput(const std::string& filename) {
    std::ifstream file(filename);
    std::string line;
    if (file.is_open()) {
        std::getline(file, line);
        file.close();
    }
    return line;
}

std::vector<std::pair<int, long long>> tryMove(const std::vector<std::pair<int, long long>>& rock, char direction, const std::set<std::pair<int, long long>>& chamber) {
    std::vector<std::pair<int, long long>> movedRock;
    movedRock.reserve(rock.size());

    for (const auto& p : rock) {
        int newX = p.first;
        long long newY = p.second;

        if (direction == '<') newX--;
        else if (direction == '>') newX++;
        else if (direction == 'v') newY--;

        if (newX < 0 || newX >= CHAMBER_WIDTH || newY < 1) {
            return {};
        }
        if (chamber.count({newX, newY})) {
            return {};
        }
        movedRock.push_back({newX, newY});
    }
    return movedRock;
}

std::vector<int> getChamberProfile(const std::set<std::pair<int, long long>>& chamber, long long highestY) {
    std::vector<int> profile(CHAMBER_WIDTH);
    for (int x = 0; x < CHAMBER_WIDTH; ++x) {
        bool found = false;
        for (long long y = highestY; y >= std::max(1LL, highestY - PROFILE_DEPTH); --y) {
            if (chamber.count({x, y})) {
                profile[x] = (int)(highestY - y);
                found = true;
                break;
            }
        }
        if (!found) {
            profile[x] = PROFILE_DEPTH + 1;
        }
    }
    return profile;
}

long long solve(const std::string& jetPattern, long long totalRocks) {
    std::set<std::pair<int, long long>> chamber;
    
    for (int x = 0; x < CHAMBER_WIDTH; ++x) {
        chamber.insert({x, 0});
    }
    
    long long highestY = 0;
    int jetLen = jetPattern.length();
    long long jetIndex = 0;
    int rockIndex = 0;
    
    std::map<std::tuple<int, int, std::vector<int>>, std::pair<long long, long long>> seenStates;
    long long additionalHeight = 0;
    long long rockNumber = 0;
    
    while (rockNumber < totalRocks) {
        const auto& shape = rockShapes[rockIndex % rockShapes.size()];
        
        int rockX = 2;
        long long rockYSpawn = highestY + 4;
        
        std::vector<std::pair<int, long long>> currentRock;
        currentRock.reserve(shape.size());
        for (const auto& p : shape) {
            currentRock.push_back({rockX + p.first, rockYSpawn + p.second});
        }
        
        while (true) {
            char jetDirChar = jetPattern[jetIndex % jetLen];
            jetIndex++;
            
            std::vector<std::pair<int, long long>> movedRockHorizontal;
            if (jetDirChar == '>') {
                movedRockHorizontal = tryMove(currentRock, '>', chamber);
            } else if (jetDirChar == '<') {
                movedRockHorizontal = tryMove(currentRock, '<', chamber);
            }

            if (!movedRockHorizontal.empty()) {
                currentRock = movedRockHorizontal;
            }
            
            std::vector<std::pair<int, long long>> movedRockDown = tryMove(currentRock, 'v', chamber);
            if (!movedRockDown.empty()) {
                currentRock = movedRockDown;
            } else {
                for (const auto& pos : currentRock) {
                    chamber.insert(pos);
                    if (pos.second > highestY) {
                        highestY = pos.second;
                    }
                }
                break;
            }
        }
        
        std::vector<int> profile = getChamberProfile(chamber, highestY);
        std::tuple<int, int, std::vector<int>> state = {rockIndex % (int)rockShapes.size(), (int)(jetIndex % jetLen), profile};
        
        if (seenStates.count(state)) {
            auto [prevRockNum, prevHeight] = seenStates[state];
            long long cycleLength = rockNumber - prevRockNum;
            long long cycleHeight = highestY - prevHeight;
            
            long long remainingRocks = totalRocks - rockNumber;
            if (remainingRocks > 0) {
                long long numCycles = remainingRocks / cycleLength;
                
                additionalHeight += numCycles * cycleHeight;
                rockNumber += numCycles * cycleLength;
            }
        } else {
            seenStates[state] = {rockNumber, highestY};
        }
        
        rockNumber++;
        rockIndex++;
    }
    
    return highestY + additionalHeight;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::string jetPattern = readInput("input.txt");
    long long totalRocks = 1000000000000LL;
    long long finalHeight = solve(jetPattern, totalRocks);
    
    std::cout << finalHeight << std::endl;
    
    return 0;
}

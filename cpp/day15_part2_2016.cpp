
#include <iostream>
#include <fstream>
#include <vector>
#include <regex>
#include <string>

struct Disc {
    int totalPositions;
    int startPosition;
};

bool checkDiscs(std::vector<Disc>& discs, int time);

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<Disc> discs;
    std::regex discRegex("Disc #(\\d+) has (\\d+) positions; at time=0, it is at position (\\d+).");

    std::string line;
    while (std::getline(file, line)) {
        std::smatch matches;
        if (std::regex_search(line, matches, discRegex)) {
            int totalPositions = std::stoi(matches[2]);
            int startPosition = std::stoi(matches[3]);
            discs.push_back({totalPositions, startPosition});
        }
    }

    discs.push_back({11, 0});

    int time = 0;
    while (true) {
        if (checkDiscs(discs, time)) {
            std::cout << time << std::endl;
            break;
        }
        time++;
    }

    return 0;
}

bool checkDiscs(std::vector<Disc>& discs, int time) {
    for (size_t i = 0; i < discs.size(); i++) {
        int position = (discs[i].startPosition + time + i + 1) % discs[i].totalPositions;
        if (position != 0) {
            return false;
        }
    }
    return true;
}

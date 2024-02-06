
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <unordered_map>

int countOrbits(std::unordered_map<std::string, std::vector<std::string> >& orbitMap, std::string start, int depth) {
    std::vector<std::string> orbits = orbitMap[start];
    if (orbits.empty()) {
        return depth;
    }
    int count = depth;
    for (std::string orbit : orbits) {
        count += countOrbits(orbitMap, orbit, depth + 1);
    }
    return count;
}

int main() {
    std::ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::string line;
    std::unordered_map<std::string, std::vector<std::string> > orbitMap;
    while (std::getline(inputFile, line)) {
        size_t pos = line.find(')');
        std::string center = line.substr(0, pos);
        std::string orbiter = line.substr(pos + 1);
        orbitMap[center].push_back(orbiter);
    }

    int totalOrbits = countOrbits(orbitMap, "COM", 0);
    std::cout << totalOrbits << std::endl;

    return 0;
}

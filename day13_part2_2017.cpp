#include <iostream>
#include <fstream>
#include <sstream>
#include <unordered_map>

struct Scanner {
    int Range;
    int Position;
    int Direction;
};

bool passThrough(std::unordered_map<int, Scanner*>& firewall, int delay);

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error reading file" << std::endl;
        return 1;
    }

    std::unordered_map<int, Scanner*> firewall;
    std::string line;
    while (std::getline(file, line)) {
        std::istringstream iss(line);
        std::string depthStr, rangeStr;
        iss >> depthStr >> rangeStr;
        int depth = std::stoi(depthStr);
        int range = std::stoi(rangeStr);
        firewall[depth] = new Scanner{range, 0, 1};
    }

    int delay = 0;
    while (!passThrough(firewall, delay)) {
        delay++;
    }

    std::cout << delay << std::endl;

    for (auto& pair : firewall) {
        delete pair.second;
    }

    return 0;
}

bool passThrough(std::unordered_map<int, Scanner*>& firewall, int delay) {
    for (auto& pair : firewall) {
        int depth = pair.first;
        Scanner* scanner = pair.second;
        if ((depth + delay) % (2 * (scanner->Range - 1)) == 0) {
            return false;
        }
    }
    return true;
}
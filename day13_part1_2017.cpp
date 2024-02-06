
#include <iostream>
#include <fstream>
#include <string>
#include <unordered_map>

struct Scanner {
    int Range;
    int Position;
    int Direction;
};

void moveScanner(Scanner* scanner) {
    if (scanner->Position == 0) {
        scanner->Direction = 1;
    } else if (scanner->Position == scanner->Range - 1) {
        scanner->Direction = -1;
    }
    scanner->Position += scanner->Direction;
}

int maxDepth(std::unordered_map<int, Scanner*> firewall) {
    int max = 0;
    for (auto const& pair : firewall) {
        if (pair.first > max) {
            max = pair.first;
        }
    }
    return max;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error reading file" << std::endl;
        return 1;
    }

    std::unordered_map<int, Scanner*> firewall;

    std::string line;
    while (std::getline(file, line)) {
        int depth = std::stoi(line.substr(0, line.find(":")));
        int range = std::stoi(line.substr(line.find(": ") + 2));
        firewall[depth] = new Scanner{range, 0, 1};
    }

    int severity = 0;
    int max_depth = maxDepth(firewall);

    for (int depth = 0; depth <= max_depth; depth++) {
        if (firewall.find(depth) != firewall.end()) {
            Scanner* scanner = firewall[depth];
            if (scanner->Position == 0) {
                severity += depth * scanner->Range;
            }
        }

        for (auto const& pair : firewall) {
            moveScanner(pair.second);
        }
    }

    std::cout << severity << std::endl;

    // Cleanup
    for (auto const& pair : firewall) {
        delete pair.second;
    }

    return 0;
}

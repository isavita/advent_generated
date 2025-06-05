
#include <iostream>
#include <fstream>
#include <string>
#include <vector>

struct Node {
    int left;
    int right;
};

int stringToInt(const std::string& s) {
    return (s[0] - 'A') * 26 * 26 + (s[1] - 'A') * 26 + (s[2] - 'A');
}

int main() {
    std::ifstream file("input.txt");
    std::string instructions;
    std::getline(file, instructions);

    std::string line;
    std::getline(file, line); // Consume the empty line

    std::vector<Node> desertMap(26 * 26 * 26);

    while (std::getline(file, line)) {
        int nameId = stringToInt(line.substr(0, 3));
        int leftId = stringToInt(line.substr(7, 3));
        int rightId = stringToInt(line.substr(12, 3));
        desertMap[nameId] = {leftId, rightId};
    }
    file.close();

    int current = stringToInt("AAA");
    const int elemToMatch = stringToInt("ZZZ");
    long long steps = 0;
    size_t instructionIndex = 0;
    size_t instructionsLength = instructions.length();

    while (current != elemToMatch) {
        char direction = instructions[instructionIndex];

        if (direction == 'R') {
            current = desertMap[current].right;
        } else { // direction == 'L'
            current = desertMap[current].left;
        }
        steps++;
        instructionIndex = (instructionIndex + 1) % instructionsLength;
    }

    std::cout << steps << std::endl;

    return 0;
}

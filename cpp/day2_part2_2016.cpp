
#include <iostream>
#include <fstream>
#include <unordered_map>

std::string getBathroomCode(std::vector<std::string> instructions);

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<std::string> instructions;
    std::string line;
    while (std::getline(file, line)) {
        instructions.push_back(line);
    }

    std::string code = getBathroomCode(instructions);
    std::cout << code << std::endl;

    return 0;
}

std::string getBathroomCode(std::vector<std::string> instructions) {
    std::unordered_map<std::string, std::unordered_map<char, std::string>> keypad = {
        {"1", {{'D', "3"}}},
        {"2", {{'R', "3"}, {'D', "6"}}},
        {"3", {{'U', "1"}, {'R', "4"}, {'D', "7"}, {'L', "2"}}},
        {"4", {{'L', "3"}, {'D', "8"}}},
        {"5", {{'R', "6"}}},
        {"6", {{'U', "2"}, {'R', "7"}, {'D', "A"}, {'L', "5"}}},
        {"7", {{'U', "3"}, {'R', "8"}, {'D', "B"}, {'L', "6"}}},
        {"8", {{'U', "4"}, {'R', "9"}, {'D', "C"}, {'L', "7"}}},
        {"9", {{'L', "8"}}},
        {"A", {{'U', "6"}, {'R', "B"}}},
        {"B", {{'U', "7"}, {'R', "C"}, {'D', "D"}, {'L', "A"}}},
        {"C", {{'U', "8"}, {'L', "B"}}},
        {"D", {{'U', "B"}}}
    };

    std::string position = "5";
    std::string code;

    for (const std::string& instruction : instructions) {
        for (char move : instruction) {
            if (keypad[position].find(move) != keypad[position].end()) {
                position = keypad[position][move];
            }
        }
        code += position;
    }

    return code;
}

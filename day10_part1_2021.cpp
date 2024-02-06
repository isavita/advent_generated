
#include <iostream>
#include <fstream>
#include <unordered_map>
#include <vector>

int checkLine(std::string line);

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    int totalScore = 0;
    std::string line;
    while (std::getline(file, line)) {
        int score = checkLine(line);
        if (score != 0) {
            totalScore += score;
        }
    }

    std::cout << totalScore << std::endl;
    return 0;
}

int checkLine(std::string line) {
    std::unordered_map<char, char> pairings = {{')', '('}, {']', '['}, {'}', '{'}, {'>', '<'}};
    std::unordered_map<char, int> scores = {{')', 3}, {']', 57}, {'}', 1197}, {'>', 25137}};
    std::vector<char> stack;

    for (char c : line) {
        switch (c) {
            case '(':
            case '[':
            case '{':
            case '<':
                stack.push_back(c);
                break;
            case ')':
            case ']':
            case '}':
            case '>':
                if (stack.empty() || stack.back() != pairings[c]) {
                    return scores[c]; // corrupted line
                }
                stack.pop_back(); // pop from stack
                break;
        }
    }
    return 0; // line is not corrupted
}

#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <map>

int main() {
    std::ifstream file("input.txt");
    if (!file) {
        std::cerr << "Unable to open file\n";
        return 1;
    }

    std::vector<std::string> lines;
    std::string line;
    while (std::getline(file, line)) {
        lines.push_back(line);
    }

    std::string message;
    for (size_t i = 0; i < lines[0].size(); ++i) {
        std::map<char, int> charCount;
        for (const auto& line : lines) {
            ++charCount[line[i]];
        }
        char maxChar = '\0';
        int maxCount = 0;
        for (const auto& pair : charCount) {
            if (pair.second > maxCount) {
                maxCount = pair.second;
                maxChar = pair.first;
            }
        }
        message += maxChar;
    }

    std::cout << "Error-corrected message: " << message << std::endl;

    return 0;
}
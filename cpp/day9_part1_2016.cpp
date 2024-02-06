
#include <iostream>
#include <fstream>
#include <regex>
#include <string>

int getDecompressedLength(const std::string& input) {
    std::regex markerRegex("\\((\\d+)x(\\d+)\\)");
    int length = 0;
    for (size_t i = 0; i < input.length();) {
        std::smatch markerMatch;
        if (std::regex_search(input.begin() + i, input.end(), markerMatch, markerRegex)) {
            int charCount = std::stoi(markerMatch.str(1));
            int repeatCount = std::stoi(markerMatch.str(2));
            length += charCount * repeatCount;
            i += markerMatch.position() + markerMatch.length() + charCount;
        } else {
            length++;
            i++;
        }
    }
    return length;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        throw std::runtime_error("Error opening file");
    }

    std::string line;
    std::getline(file, line);
    int decompressedLength = getDecompressedLength(line);

    std::cout << decompressedLength << std::endl;

    return 0;
}

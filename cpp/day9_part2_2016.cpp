#include <iostream>
#include <fstream>
#include <string>
#include <cctype>

long long decompressLength(const std::string &data, bool versionTwo) {
    long long length = 0;
    size_t i = 0;

    while (i < data.size()) {
        if (data[i] == '(') {
            size_t closeParen = data.find(')', i);
            auto marker = data.substr(i + 1, closeParen - i - 1);
            size_t xPos = marker.find('x');
            int charsToRepeat = std::stoi(marker.substr(0, xPos));
            int repeatCount = std::stoi(marker.substr(xPos + 1));

            if (versionTwo) {
                length += decompressLength(data.substr(closeParen + 1, charsToRepeat), versionTwo) * repeatCount;
            } else {
                length += charsToRepeat * repeatCount;
            }

            i = closeParen + 1 + charsToRepeat;
        } else {
            length++;
            i++;
        }
    }

    return length;
}

int main() {
    std::ifstream inputFile("input.txt");
    std::string data;

    if (inputFile) {
        std::getline(inputFile, data);
        inputFile.close();
    } else {
        std::cerr << "Unable to open input file." << std::endl;
        return 1;
    }

    long long partOneLength = decompressLength(data, false);
    long long partTwoLength = decompressLength(data, true);

    std::cout << "Part One Decompressed Length: " << partOneLength << std::endl;
    std::cout << "Part Two Decompressed Length: " << partTwoLength << std::endl;

    return 0;
}
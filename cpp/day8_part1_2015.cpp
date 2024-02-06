#include <iostream>
#include <fstream>

int calculateMemoryLength(std::string s) {
    int length = 0;
    bool inEscape = false;
    int hexCount = 0;

    for (int i = 1; i < s.length() - 1; i++) {
        if (hexCount > 0) {
            hexCount--;
        } else if (inEscape) {
            if (s[i] == 'x') {
                hexCount = 2;
            }
            inEscape = false;
            length++;
        } else if (s[i] == '\\') {
            inEscape = true;
        } else {
            length++;
        }
    }
    return length;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    int totalDiff = 0;
    std::string line;
    while (std::getline(file, line)) {
        int codeLength = line.length();
        int memoryLength = calculateMemoryLength(line);
        totalDiff += codeLength - memoryLength;
    }

    file.close();

    std::cout << totalDiff << std::endl;

    return 0;
}
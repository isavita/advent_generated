#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream input("input.txt");
    std::string line;
    int codeLength = 0;
    int memoryLength = 0;
    int encodedLength = 0;

    while (std::getline(input, line)) {
        codeLength += line.length();

        // Calculate memory length
        int i = 1;
        while (i < line.length() - 1) {
            if (line[i] == '\\') {
                if (line[i + 1] == '\\' || line[i + 1] == '"') {
                    memoryLength++;
                    i += 2;
                } else if (line[i + 1] == 'x') {
                    memoryLength++;
                    i += 4;
                }
            } else {
                memoryLength++;
                i++;
            }
        }

        // Calculate encoded length
        encodedLength += 2; // For the surrounding double quotes
        for (char c : line) {
            if (c == '"' || c == '\\') {
                encodedLength += 2;
            } else {
                encodedLength++;
            }
        }
    }

    std::cout << encodedLength - codeLength << std::endl;

    return 0;
}
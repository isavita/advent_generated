
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>

int main() {
    std::ifstream inputFile("input.txt");
    int steps;
    inputFile >> steps;

    std::vector<int> buffer = {0};
    int currentPos = 0;

    for (int i = 1; i <= 2017; i++) {
        currentPos = (currentPos + steps) % buffer.size();
        buffer.insert(buffer.begin() + currentPos + 1, i);
        currentPos++;
    }

    int result;
    for (size_t i = 0; i < buffer.size(); i++) {
        if (buffer[i] == 2017) {
            result = buffer[(i + 1) % buffer.size()];
            break;
        }
    }

    std::cout << result << std::endl;

    return 0;
}

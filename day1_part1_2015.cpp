#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream inputFile("input.txt");
    if (inputFile.is_open()) {
        std::string line;
        int floor = 0;
        while (std::getline(inputFile, line)) {
            for (char& c : line) {
                if (c == '(') {
                    floor++;
                } else if (c == ')') {
                    floor--;
                }
            }
        }
        std::cout << floor << std::endl;
        inputFile.close();
    }
    return 0;
}
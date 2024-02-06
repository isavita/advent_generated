#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream file("input.txt");
    std::string line;
    std::getline(file, line);

    int floor = 0;
    int position = 0;
    bool basementFound = false;

    for (char& c : line) {
        position++;
        if (c == '(') {
            floor++;
        } else if (c == ')') {
            floor--;
        }

        if (floor == -1 && !basementFound) {
            std::cout << position << std::endl;
            basementFound = true;
        }
    }

    std::cout << floor << std::endl;

    return 0;
}
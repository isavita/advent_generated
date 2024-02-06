#include <iostream>
#include <fstream>
#include <string>
#include <sstream>

bool validatePassword(std::string policy, std::string password) {
    int min, max;
    char character;
    std::istringstream iss(policy);
    iss >> min;
    iss.ignore(1);
    iss >> max;
    iss.ignore(1);
    iss >> character;

    int count = 0;
    for (char c : password) {
        if (c == character) {
            count++;
        }
    }
    return count >= min && count <= max;
}

int main() {
    int validCount = 0;
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::string line;
    while (std::getline(file, line)) {
        size_t i = line.find(":");
        if (i != std::string::npos) {
            std::string policy = line.substr(0, i);
            std::string password = line.substr(i + 2);
            if (validatePassword(policy, password)) {
                validCount++;
            }
        }
    }

    std::cout << validCount << std::endl;

    return 0;
}

#include <iostream>
#include <fstream>
#include <sstream>

bool validatePassword(std::string policy, std::string password) {
    int min = 0, max = 0;
    char character = 0;
    std::stringstream ss(policy);
    ss >> min;
    ss.ignore(1, '-');
    ss >> max;
    ss.ignore(1, ' ');
    ss >> character;
    return (password[min - 1] == character) != (password[max - 1] == character);
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
        size_t pos = line.find(":");
        if (pos != std::string::npos) {
            std::string policy = line.substr(0, pos);
            std::string password = line.substr(pos + 2);
            if (validatePassword(policy, password)) {
                validCount++;
            }
        }
    }

    std::cout << validCount << std::endl;
    return 0;
}

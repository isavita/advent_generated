
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        throw std::runtime_error("Failed to open file");
    }

    std::string line;
    int count = 0;
    while (std::getline(file, line)) {
        std::vector<std::string> parts;
        std::istringstream iss(line);
        for (std::string part; std::getline(iss, part, '|'); ) {
            parts.push_back(part);
        }
        std::string output = parts[1];
        std::istringstream oss(output);
        for (std::string digit; oss >> digit; ) {
            switch (digit.length()) {
                case 2:
                case 4:
                case 3:
                case 7:
                    count++;
                    break;
            }
        }
    }

    std::cout << count << std::endl;

    return 0;
}

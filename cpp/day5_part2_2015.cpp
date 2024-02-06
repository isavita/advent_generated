#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>

bool passesRule1(std::string line) {
    for (int i = 0; i < line.length() - 2; i++) {
        std::string toMatch = line.substr(i, 2);
        for (int j = i + 2; j < line.length() - 1; j++) {
            if (line.substr(j, 2) == toMatch) {
                return true;
            }
        }
    }
    return false;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::string input;
    std::string line;
    while (std::getline(file, line)) {
        input += line + "\n";
    }

    int nice = 0;
    std::vector<std::string> lines;
    size_t pos = 0;
    while ((pos = input.find("\n")) != std::string::npos) {
        std::string line = input.substr(0, pos);
        lines.push_back(line);
        input.erase(0, pos + 1);
    }

    for (auto line : lines) {
        bool rule1 = passesRule1(line);

        bool rule2 = false;
        for (int i = 0; i < line.length() - 2; i++) {
            if (line[i] == line[i + 2]) {
                rule2 = true;
                break;
            }
        }

        if (rule1 && rule2) {
            nice++;
        }
    }

    std::cout << nice << std::endl;

    return 0;
}
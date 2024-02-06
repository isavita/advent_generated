
#include <iostream>
#include <fstream>
#include <vector>

std::string getBathroomCode(std::vector<std::string> instructions) {
    int keypad[3][3] = {
        {1, 2, 3},
        {4, 5, 6},
        {7, 8, 9}
    };
    int x = 1, y = 1; // Start at '5'
    std::string code = "";

    for (const auto& instruction : instructions) {
        for (char move : instruction) {
            switch (move) {
                case 'U':
                    if (x > 0) {
                        x--;
                    }
                    break;
                case 'D':
                    if (x < 2) {
                        x++;
                    }
                    break;
                case 'L':
                    if (y > 0) {
                        y--;
                    }
                    break;
                case 'R':
                    if (y < 2) {
                        y++;
                    }
                    break;
            }
        }
        code += std::to_string(keypad[x][y]);
    }

    return code;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        return 1;
    }

    std::vector<std::string> instructions;
    std::string line;
    while (std::getline(file, line)) {
        instructions.push_back(line);
    }

    std::string code = getBathroomCode(instructions);
    std::cout << code << std::endl;

    return 0;
}

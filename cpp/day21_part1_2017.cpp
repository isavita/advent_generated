
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <unordered_map>
#include <string>

std::unordered_map<std::string, std::string> rules;
std::vector<std::string> grid;

std::string rotate(const std::string& input) {
    std::vector<std::string> parts;
    std::istringstream iss(input);
    std::string part;
    while (std::getline(iss, part, '/')) {
        parts.push_back(part);
    }
    int size = parts.size();
    std::vector<std::string> newParts(size);
    for (int x = 0; x < size; ++x) {
        std::string newRow;
        for (int y = size - 1; y >= 0; --y) {
            newRow += parts[y][x];
        }
        newParts[x] = newRow;
    }
    std::ostringstream oss;
    for (const auto& row : newParts) {
        oss << row << "/";
    }
    std::string result = oss.str();
    return result.substr(0, result.size() - 1);
}

std::string flip(const std::string& input) {
    std::vector<std::string> parts;
    std::istringstream iss(input);
    std::string part;
    while (std::getline(iss, part, '/')) {
        std::reverse(part.begin(), part.end());
        parts.push_back(part);
    }
    std::ostringstream oss;
    for (const auto& row : parts) {
        oss << row << "/";
    }
    std::string result = oss.str();
    return result.substr(0, result.size() - 1);
}

std::string enhance(const std::string& input) {
    std::string current = input;
    for (int i = 0; i < 4; ++i) {
        if (rules.find(current) != rules.end()) {
            return rules[current];
        }
        current = rotate(current);
    }
    current = flip(current);
    for (int i = 0; i < 4; ++i) {
        if (rules.find(current) != rules.end()) {
            return rules[current];
        }
        current = rotate(current);
    }
    return "";
}

int main() {
    std::ifstream file("input.txt");
    std::string line;
    while (std::getline(file, line)) {
        size_t pos = line.find(" => ");
        rules[line.substr(0, pos)] = line.substr(pos + 4);
    }

    grid = {".#.", "..#", "###"};

    for (int i = 0; i < 5; ++i) {
        int newSize, subSize;
        if (grid.size() % 2 == 0) {
            subSize = 2;
            newSize = grid.size() / 2 * 3;
        } else {
            subSize = 3;
            newSize = grid.size() / 3 * 4;
        }

        std::vector<std::string> newGrid(newSize, std::string(newSize, ' '));

        for (int y = 0; y < grid.size(); y += subSize) {
            for (int x = 0; x < grid.size(); x += subSize) {
                std::vector<std::string> square;
                for (int dy = 0; dy < subSize; ++dy) {
                    square.push_back(grid[y + dy].substr(x, subSize));
                }
                std::ostringstream oss;
                for (const auto& row : square) {
                    oss << row << "/";
                }
                std::string newSquare = enhance(oss.str().substr(0, oss.str().size() - 1));
                std::istringstream iss(newSquare);
                std::string row;
                int dy = 0;
                while (std::getline(iss, row, '/')) {
                    newGrid[y / subSize * (subSize + 1) + dy] += row;
                    ++dy;
                }
            }
        }
        grid = newGrid;
    }

    int count = 0;
    for (const auto& row : grid) {
        for (char pixel : row) {
            if (pixel == '#') {
                ++count;
            }
        }
    }
    std::cout << count << std::endl;
    return 0;
}

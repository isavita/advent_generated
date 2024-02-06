#include <iostream>
#include <fstream>
#include <vector>
#include <numeric>
#include <unordered_map>
#include <cctype>

std::vector<std::vector<char>> readFileToMatrix(std::string filePath);
int sumOfPartNumbers(std::vector<std::vector<char>> matrix);
std::pair<int, int> extractNumber(std::vector<std::vector<char>> matrix, int x, int y);
bool isAdjacentToSymbol(std::vector<std::vector<char>> matrix, int x, int y, int length);
bool checkAdjacent(std::vector<std::vector<char>> matrix, int x, int y);

int main() {
    // Read the file and convert it into a 2D vector of chars.
    std::vector<std::vector<char>> matrix = readFileToMatrix("input.txt");

    // Calculate the sum of all part numbers.
    int sum = sumOfPartNumbers(matrix);
    std::cout << sum << std::endl;

    return 0;
}

std::vector<std::vector<char>> readFileToMatrix(std::string filePath) {
    std::ifstream file(filePath);
    std::string line;
    std::vector<std::vector<char>> matrix;

    while (std::getline(file, line)) {
        std::vector<char> row(line.begin(), line.end());
        matrix.push_back(row);
    }

    return matrix;
}

int sumOfPartNumbers(std::vector<std::vector<char>> matrix) {
    int sum = 0;
    std::vector<std::vector<bool>> visited(matrix.size(), std::vector<bool>(matrix[0].size(), false));

    for (int y = 0; y < matrix.size(); y++) {
        for (int x = 0; x < matrix[y].size(); x++) {
            if (!visited[y][x] && std::isdigit(matrix[y][x])) {
                auto [number, length] = extractNumber(matrix, x, y);
                if (isAdjacentToSymbol(matrix, x, y, length)) {
                    sum += number;
                }
                // Mark all digits of this number as visited.
                for (int i = 0; i < length; i++) {
                    visited[y][x + i] = true;
                }
            }
        }
    }

    return sum;
}

std::pair<int, int> extractNumber(std::vector<std::vector<char>> matrix, int x, int y) {
    std::string numberStr;
    while (x < matrix[y].size() && std::isdigit(matrix[y][x])) {
        numberStr += matrix[y][x];
        x++;
    }
    int number = std::stoi(numberStr);
    return {number, numberStr.length()};
}

bool isAdjacentToSymbol(std::vector<std::vector<char>> matrix, int x, int y, int length) {
    for (int i = 0; i < length; i++) {
        if (checkAdjacent(matrix, x + i, y)) {
            return true;
        }
    }
    return false;
}

bool checkAdjacent(std::vector<std::vector<char>> matrix, int x, int y) {
    for (int dy = -1; dy <= 1; dy++) {
        for (int dx = -1; dx <= 1; dx++) {
            int adjX = x + dx;
            int adjY = y + dy;
            if (adjY >= 0 && adjY < matrix.size() && adjX >= 0 && adjX < matrix[adjY].size()) {
                if (!std::isdigit(matrix[adjY][adjX]) && matrix[adjY][adjX] != '.') {
                    return true;
                }
            }
        }
    }
    return false;
}
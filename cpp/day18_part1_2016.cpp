
#include <iostream>
#include <fstream>
#include <string>

int countChar(std::string str, char c) {
    int count = 0;
    for (char ch : str) {
        if (ch == c) {
            count++;
        }
    }
    return count;
}

bool isTrap(int left, int center, int right, std::string row) {
    char l = (left < 0 || left >= row.size()) ? '.' : row[left];
    char c = row[center];
    char r = (right < 0 || right >= row.size()) ? '.' : row[right];

    return ((l == '^' && c == '^' && r == '.') ||
            (c == '^' && r == '^' && l == '.') ||
            (l == '^' && c == '.' && r == '.') ||
            (r == '^' && c == '.' && l == '.'));
}

int countSafeTiles(std::string firstRow, int totalRows) {
    std::string currentRow = firstRow;
    int safeCount = countChar(currentRow, '.');

    for (int i = 1; i < totalRows; i++) {
        std::string nextRow = "";
        for (int j = 0; j < currentRow.size(); j++) {
            if (isTrap(j - 1, j, j + 1, currentRow)) {
                nextRow += "^";
            } else {
                nextRow += ".";
                safeCount++;
            }
        }
        currentRow = nextRow;
    }
    return safeCount;
}

std::string readFirstRow(std::string filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Failed to open file");
    }

    std::string firstRow;
    if (std::getline(file, firstRow)) {
        return firstRow;
    }

    throw std::runtime_error("Failed to read the first row");
}

int main() {
    std::string firstRow = readFirstRow("input.txt");
    const int totalRows = 40;
    int safeTilesCount = countSafeTiles(firstRow, totalRows);
    std::cout << safeTilesCount << std::endl;

    return 0;
}

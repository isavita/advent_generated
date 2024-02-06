
#include <iostream>
#include <fstream>
#include <string>

int totalRows = 400000;

std::string readFirstRow(std::string filename);
int countSafeTiles(std::string firstRow, int totalRows);
bool isTrap(int left, int center, int right, std::string row);
char safeIfOutOfBounds(int index, std::string row);
int countChar(std::string str, char character);

int main() {
    std::string firstRow = readFirstRow("input.txt");
    int safeTilesCount = countSafeTiles(firstRow, totalRows);
    std::cout << safeTilesCount << std::endl;
    return 0;
}

std::string readFirstRow(std::string filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Failed to open file");
    }

    std::string firstRow;
    std::getline(file, firstRow);

    file.close();
    return firstRow;
}

int countSafeTiles(std::string firstRow, int totalRows) {
    std::string currentRow = firstRow;
    int safeCount = countChar(currentRow, '.');

    for (int i = 1; i < totalRows; i++) {
        std::string nextRow;
        for (int j = 0; j < currentRow.length(); j++) {
            if (isTrap(j-1, j, j+1, currentRow)) {
                nextRow += '^';
            } else {
                nextRow += '.';
                safeCount++;
            }
        }
        currentRow = nextRow;
    }
    return safeCount;
}

bool isTrap(int left, int center, int right, std::string row) {
    char l = safeIfOutOfBounds(left, row);
    char c = row[center];
    char r = safeIfOutOfBounds(right, row);

    return (l == '^' && c == '^' && r == '.') ||
        (c == '^' && r == '^' && l == '.') ||
        (l == '^' && c == '.' && r == '.') ||
        (r == '^' && c == '.' && l == '.');
}

char safeIfOutOfBounds(int index, std::string row) {
    if (index < 0 || index >= row.length()) {
        return '.';
    }
    return row[index];
}

int countChar(std::string str, char character) {
    int count = 0;
    for (char c : str) {
        if (c == character) {
            count++;
        }
    }
    return count;
}

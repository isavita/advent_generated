
#include <iostream>
#include <fstream>
#include <vector>
#include <regex>

const int screenWidth = 50;
const int screenHeight = 6;

void processInstruction(std::string instruction, std::vector<std::vector<bool> >& screen);
void rect(std::vector<std::vector<bool> >& screen, int a, int b);
void rotateRow(std::vector<std::vector<bool> >& screen, int row, int shift);
void rotateColumn(std::vector<std::vector<bool> >& screen, int col, int shift);
void displayScreen(std::vector<std::vector<bool> >& screen);
int countLitPixels(std::vector<std::vector<bool> >& screen);

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<std::vector<bool> > screen(screenHeight, std::vector<bool>(screenWidth, false));

    std::string line;
    while (std::getline(file, line)) {
        processInstruction(line, screen);
    }

    displayScreen(screen);

    return 0;
}

void displayScreen(std::vector<std::vector<bool> >& screen) {
    for (const auto& row : screen) {
        for (const bool pixel : row) {
            std::cout << (pixel ? "#" : ".");
        }
        std::cout << std::endl;
    }
}

void processInstruction(std::string instruction, std::vector<std::vector<bool> >& screen) {
    std::regex rectRegex("rect (\\d+)x(\\d+)");
    std::regex rotateRowRegex("rotate row y=(\\d+) by (\\d+)");
    std::regex rotateColumnRegex("rotate column x=(\\d+) by (\\d+)");

    if (std::regex_match(instruction, rectRegex)) {
        std::smatch matches;
        std::regex_search(instruction, matches, rectRegex);
        int a = std::stoi(matches[1]);
        int b = std::stoi(matches[2]);
        rect(screen, a, b);
    } else if (std::regex_match(instruction, rotateRowRegex)) {
        std::smatch matches;
        std::regex_search(instruction, matches, rotateRowRegex);
        int a = std::stoi(matches[1]);
        int b = std::stoi(matches[2]);
        rotateRow(screen, a, b);
    } else if (std::regex_match(instruction, rotateColumnRegex)) {
        std::smatch matches;
        std::regex_search(instruction, matches, rotateColumnRegex);
        int a = std::stoi(matches[1]);
        int b = std::stoi(matches[2]);
        rotateColumn(screen, a, b);
    }
}

void rect(std::vector<std::vector<bool> >& screen, int a, int b) {
    for (int y = 0; y < b; y++) {
        for (int x = 0; x < a; x++) {
            screen[y][x] = true;
        }
    }
}

void rotateRow(std::vector<std::vector<bool> >& screen, int row, int shift) {
    std::vector<bool> temp(screenWidth);
    for (int i = 0; i < screenWidth; i++) {
        temp[(i + shift) % screenWidth] = screen[row][i];
    }
    screen[row] = temp;
}

void rotateColumn(std::vector<std::vector<bool> >& screen, int col, int shift) {
    std::vector<bool> temp(screenHeight);
    for (int i = 0; i < screenHeight; i++) {
        temp[(i + shift) % screenHeight] = screen[i][col];
    }
    for (int i = 0; i < screenHeight; i++) {
        screen[i][col] = temp[i];
    }
}

int countLitPixels(std::vector<std::vector<bool> >& screen) {
    int count = 0;
    for (const auto& row : screen) {
        for (const bool pixel : row) {
            if (pixel) {
                count++;
            }
        }
    }
    return count;
}

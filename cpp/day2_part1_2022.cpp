
#include <iostream>
#include <fstream>

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    char opponent, yourMove;
    int totalScore = 0;

    while (file >> opponent >> yourMove) {
        int score = 0;
        if (yourMove == 'X') {
            score = 1;
        } else if (yourMove == 'Y') {
            score = 2;
        } else if (yourMove == 'Z') {
            score = 3;
        }

        if ((opponent == 'A' && yourMove == 'Y') || (opponent == 'B' && yourMove == 'Z') || (opponent == 'C' && yourMove == 'X')) {
            score += 6;
        } else if (opponent == 'A' && yourMove == 'X' || opponent == 'B' && yourMove == 'Y' || opponent == 'C' && yourMove == 'Z') {
            score += 3;
        }

        totalScore += score;
    }

    file.close();

    std::cout << totalScore << std::endl;

    return 0;
}

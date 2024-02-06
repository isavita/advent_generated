
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    int totalScore = 0;
    std::string line;

    while (std::getline(file, line)) {
        char opponent = line[0];
        char roundEnd = line[2];

        char yourMove = ' ';
        if (roundEnd == 'X') {
            if (opponent == 'A') {
                yourMove = 'Z';
            } else if (opponent == 'B') {
                yourMove = 'X';
            } else {
                yourMove = 'Y';
            }
        } else if (roundEnd == 'Y') {
            if (opponent == 'A') {
                yourMove = 'X';
            } else if (opponent == 'B') {
                yourMove = 'Y';
            } else {
                yourMove = 'Z';
            }
        } else {
            if (opponent == 'A') {
                yourMove = 'Y';
            } else if (opponent == 'B') {
                yourMove = 'Z';
            } else {
                yourMove = 'X';
            }
        }

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
        } else if ((opponent == 'A' && yourMove == 'X') || (opponent == 'B' && yourMove == 'Y') || (opponent == 'C' && yourMove == 'Z')) {
            score += 3;
        }

        totalScore += score;
    }

    file.close();

    std::cout << totalScore << std::endl;

    return 0;
}

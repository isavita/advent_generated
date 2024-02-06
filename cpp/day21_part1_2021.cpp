
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream inputFile("input.txt");
    std::string line;
    int player1Start, player2Start;
    std::getline(inputFile, line);
    player1Start = std::stoi(line.substr(28));
    std::getline(inputFile, line);
    player2Start = std::stoi(line.substr(28));

    int player1Pos = player1Start;
    int player2Pos = player2Start;
    int player1Score = 0;
    int player2Score = 0;
    int dieRoll = 1;
    int rollCount = 0;

    while (true) {
        // Player 1
        int rolls = dieRoll % 100 + (dieRoll + 1) % 100 + (dieRoll + 2) % 100;
        rollCount += 3;
        dieRoll += 3;

        player1Pos = (player1Pos + rolls - 1) % 10 + 1;
        player1Score += player1Pos;

        if (player1Score >= 1000) {
            std::cout << "Result: " << player2Score * rollCount << std::endl;
            break;
        }

        // Player 2
        rolls = dieRoll % 100 + (dieRoll + 1) % 100 + (dieRoll + 2) % 100;
        rollCount += 3;
        dieRoll += 3;

        player2Pos = (player2Pos + rolls - 1) % 10 + 1;
        player2Score += player2Pos;

        if (player2Score >= 1000) {
            std::cout << player1Score * rollCount << std::endl;
            break;
        }
    }

    return 0;
}


#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <numeric>

bool checkSequence(std::vector<int>& scoreboard, std::vector<int>& sequence) {
    if (scoreboard.size() < sequence.size()) {
        return false;
    }
    int start = scoreboard.size() - sequence.size();
    for (int i = 0; i < sequence.size(); i++) {
        if (scoreboard[start + i] != sequence[i]) {
            return false;
        }
    }
    return true;
}

int main() {
    std::ifstream file("input.txt");
    std::string input;
    std::getline(file, input);

    std::vector<int> scoreboard = {3, 7};
    int elf1 = 0, elf2 = 1;
    int inputLen = input.size();
    std::vector<int> inputSequence(inputLen);

    for (int i = 0; i < inputLen; i++) {
        inputSequence[i] = input[i] - '0';
    }

    while (true) {
        int newScore = scoreboard[elf1] + scoreboard[elf2];
        if (newScore >= 10) {
            scoreboard.push_back(newScore / 10);
            if (checkSequence(scoreboard, inputSequence)) {
                break;
            }
        }
        scoreboard.push_back(newScore % 10);
        if (checkSequence(scoreboard, inputSequence)) {
            break;
        }

        elf1 = (elf1 + scoreboard[elf1] + 1) % scoreboard.size();
        elf2 = (elf2 + scoreboard[elf2] + 1) % scoreboard.size();
    }

    std::cout << scoreboard.size() - inputLen << std::endl;

    return 0;
}

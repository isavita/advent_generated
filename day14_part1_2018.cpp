
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <numeric>

int main() {
    std::ifstream file("input.txt");
    int input;
    file >> input;

    std::vector<int> scoreboard = {3, 7};
    int elf1 = 0, elf2 = 1;

    while (scoreboard.size() < (size_t)(input + 10)) {
        int newScore = scoreboard[elf1] + scoreboard[elf2];
        if (newScore >= 10) {
            scoreboard.push_back(newScore / 10);
        }
        scoreboard.push_back(newScore % 10);

        elf1 = (elf1 + scoreboard[elf1] + 1) % scoreboard.size();
        elf2 = (elf2 + scoreboard[elf2] + 1) % scoreboard.size();
    }

    for (int i = input; i < input + 10; i++) {
        std::cout << scoreboard[i];
    }
    std::cout << std::endl;

    return 0;
}

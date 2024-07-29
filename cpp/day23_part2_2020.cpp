#include <iostream>
#include <fstream>
#include <vector>

const int totalCups = 1000000;
const int totalMoves = 10000000;

int main() {
    std::ifstream file("input.txt");
    std::string input;
    file >> input;

    std::vector<int> cups(totalCups + 1);
    int lastCup;

    for (size_t i = 0; i < input.size(); ++i) {
        int cup = input[i] - '0';
        if (i > 0) cups[lastCup] = cup;
        lastCup = cup;
    }

    for (int i = input.size() + 1; i <= totalCups; ++i) {
        cups[lastCup] = i;
        lastCup = i;
    }
    cups[lastCup] = input[0] - '0';

    int currentCup = input[0] - '0';
    for (int i = 0; i < totalMoves; ++i) {
        int pickup1 = cups[currentCup];
        int pickup2 = cups[pickup1];
        int pickup3 = cups[pickup2];

        cups[currentCup] = cups[pickup3];

        int destinationCup = currentCup - 1;
        if (destinationCup == 0) destinationCup = totalCups;
        while (destinationCup == pickup1 || destinationCup == pickup2 || destinationCup == pickup3) {
            destinationCup--;
            if (destinationCup == 0) destinationCup = totalCups;
        }

        cups[pickup3] = cups[destinationCup];
        cups[destinationCup] = pickup1;

        currentCup = cups[currentCup];
    }

    long long cup1 = cups[1];
    long long cup2 = cups[cup1];
    std::cout << cup1 * cup2 << std::endl;

    return 0;
}
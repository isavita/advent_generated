#include <iostream>
#include <fstream>
#include <vector>
#include <unordered_map>

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        throw std::runtime_error("Error opening file");
    }

    std::string input;
    std::getline(file, input);

    std::vector<int> cups(input.size() + 1);
    int currentCup;
    for (int i = 0; i < input.size(); i++) {
        int cup = input[i] - '0';
        if (i == 0) {
            currentCup = cup;
        }
        if (i < input.size() - 1) {
            int nextCup = input[i + 1] - '0';
            cups[cup] = nextCup;
        }
    }
    int firstCup = input[0] - '0';
    int lastCup = input[input.size() - 1] - '0';
    cups[lastCup] = firstCup;

    for (int i = 0; i < 100; i++) {
        int pickup1 = cups[currentCup];
        int pickup2 = cups[pickup1];
        int pickup3 = cups[pickup2];

        cups[currentCup] = cups[pickup3];

        int destinationCup = currentCup - 1;
        if (destinationCup < 1) {
            destinationCup = input.size();
        }
        while (destinationCup == pickup1 || destinationCup == pickup2 || destinationCup == pickup3) {
            destinationCup--;
            if (destinationCup < 1) {
                destinationCup = input.size();
            }
        }

        cups[pickup3] = cups[destinationCup];
        cups[destinationCup] = pickup1;

        currentCup = cups[currentCup];
    }

    int cup = cups[1];
    while (cup != 1) {
        std::cout << cup;
        cup = cups[cup];
        if (cup == 1) {
            break;
        }
    }
    std::cout << std::endl;

    return 0;
}
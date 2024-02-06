
#include <iostream>
#include <fstream>
#include <vector>
#include <string>

std::vector<int> repeatInput(std::string input, int times) {
    std::vector<int> digits(input.size() * times);
    for (int t = 0; t < times; t++) {
        for (int i = 0; i < input.size(); i++) {
            digits[t * input.size() + i] = input[i] - '0';
        }
    }
    return digits;
}

int main() {
    std::ifstream file("input.txt");
    std::string input;
    std::getline(file, input);

    std::vector<int> repeatedInput = repeatInput(input, 10000);

    int offset = std::stoi(input.substr(0, 7));

    for (int phase = 0; phase < 100; phase++) {
        int sum = 0;
        for (int i = repeatedInput.size() - 1; i >= offset; i--) {
            sum += repeatedInput[i];
            repeatedInput[i] = sum % 10;
        }
    }

    for (int i = offset; i < offset + 8; i++) {
        std::cout << repeatedInput[i];
    }
    std::cout << std::endl;

    return 0;
}

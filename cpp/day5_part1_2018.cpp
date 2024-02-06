#include <iostream>
#include <fstream>

std::string react(std::string polymer) {
    for (int i = 0; i < polymer.length() - 1; i++) {
        if (polymer[i] != polymer[i + 1] &&
            (polymer[i] + 32 == polymer[i + 1] ||
             polymer[i] - 32 == polymer[i + 1])) {
            return react(polymer.substr(0, i) + polymer.substr(i + 2));
        }
    }
    return polymer;
}

int main() {
    std::ifstream file("input.txt");
    std::string polymer;

    std::getline(file, polymer);

    std::string result = react(polymer);
    std::cout << result.length() << std::endl;

    return 0;
}
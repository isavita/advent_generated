#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "File reading error" << std::endl;
        return 1;
    }

    std::string input;
    std::getline(file, input);
    file.close();

    int halfway = input.length() / 2;
    int sum = 0;

    for (int i = 0; i < input.length(); i++) {
        int next = (i + halfway) % input.length();
        if (input[i] == input[next]) {
            sum += input[i] - '0';
        }
    }

    std::cout << sum << std::endl;

    return 0;
}
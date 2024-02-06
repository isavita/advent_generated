
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream file("input.txt");
    std::string input;
    std::getline(file, input);

    int sum = 0;

    for (int i = 0; i < input.length(); i++) {
        int next = (i + 1) % input.length();
        if (input[i] == input[next]) {
            sum += input[i] - '0';
        }
    }

    std::cout << sum << std::endl;

    return 0;
}

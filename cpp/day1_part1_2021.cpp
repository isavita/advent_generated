
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    int prev, current, count = 0;
    while (file >> current) {
        if (prev != 0 && current > prev) {
            count++;
        }
        prev = current;
    }

    std::cout << count << std::endl;

    return 0;
}

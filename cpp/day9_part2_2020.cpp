
#include <iostream>
#include <fstream>
#include <vector>

const int invalidNumber = 14360655;

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<int> numbers;
    int n;
    while (file >> n) {
        numbers.push_back(n);
    }

    for (int i = 0; i < numbers.size(); i++) {
        int sum = numbers[i];
        int min = numbers[i];
        int max = numbers[i];
        for (int j = i + 1; j < numbers.size(); j++) {
            sum += numbers[j];
            if (numbers[j] < min) {
                min = numbers[j];
            }
            if (numbers[j] > max) {
                max = numbers[j];
            }
            if (sum == invalidNumber) {
                std::cout << min + max << std::endl;
                return 0;
            } else if (sum > invalidNumber) {
                break;
            }
        }
    }

    return 0;
}

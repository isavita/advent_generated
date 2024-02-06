#include <iostream>
#include <fstream>
#include <vector>

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<int> expenses;
    int num;
    while (file >> num) {
        expenses.push_back(num);
    }

    for (int i = 0; i < expenses.size(); i++) {
        for (int j = i + 1; j < expenses.size(); j++) {
            for (int k = j + 1; k < expenses.size(); k++) {
                if (expenses[i] + expenses[j] + expenses[k] == 2020) {
                    std::cout << expenses[i] * expenses[j] * expenses[k] << std::endl;
                    return 0;
                }
            }
        }
    }

    return 0;
}
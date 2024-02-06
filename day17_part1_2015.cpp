#include <iostream>
#include <fstream>
#include <vector>

int countCombinations(std::vector<int>& containers, int target, int index) {
    if (target == 0) {
        return 1;
    }
    if (target < 0 || index >= containers.size()) {
        return 0;
    }
    return countCombinations(containers, target - containers[index], index + 1) +
           countCombinations(containers, target, index + 1);
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<int> containers;
    int size;
    while (file >> size) {
        containers.push_back(size);
    }

    file.close();

    std::cout << countCombinations(containers, 150, 0) << std::endl;

    return 0;
}
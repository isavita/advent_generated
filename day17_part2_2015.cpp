#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>

int main() {
    std::ifstream input("input.txt");
    int target = 150;
    std::vector<int> containers;
    int container;
    while (input >> container) {
        containers.push_back(container);
    }
    int ways = 0;
    int minContainers = containers.size();
    for (int i = 1; i < (1 << containers.size()); ++i) {
        int sum = 0;
        int count = 0;
        for (int j = 0; j < containers.size(); ++j) {
            if (i & (1 << j)) {
                sum += containers[j];
                count++;
            }
        }
        if (sum == target) {
            ways += (count == minContainers);
        }
        if (sum == target && count < minContainers) {
            minContainers = count;
            ways = 1;
        }
    }
    std::cout << ways << std::endl;
    return 0;
}
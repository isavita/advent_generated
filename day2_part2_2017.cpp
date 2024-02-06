#include <iostream>
#include <fstream>
#include <vector>
#include <sstream>

int main() {
    std::ifstream inputFile("input.txt");
    std::string line;
    int sum = 0;

    while (std::getline(inputFile, line)) {
        std::vector<int> nums;
        std::istringstream iss(line);
        int num;
        
        while (iss >> num) {
            nums.push_back(num);
        }

        for (int i = 0; i < nums.size(); i++) {
            for (int j = 0; j < nums.size(); j++) {
                if (i != j && nums[i] % nums[j] == 0) {
                    sum += nums[i] / nums[j];
                }
            }
        }
    }

    std::cout << sum << std::endl;

    return 0;
}
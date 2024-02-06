#include <iostream>
#include <fstream>
#include <vector>
#include <unordered_set>
#include <sstream>

int main() {
    std::ifstream inputFile("input.txt");
    std::vector<int> banks;
    std::unordered_set<std::string> seen;
    int cycles = 0;

    if (inputFile.is_open()) {
        std::string line;
        while (std::getline(inputFile, line)) {
            std::istringstream iss(line);
            int num;
            while (iss >> num) {
                banks.push_back(num);
            }
        }
        inputFile.close();
    } else {
        std::cerr << "Unable to open file";
        return 1;
    }

    while (true) {
        std::ostringstream oss;
        for (int num : banks) {
            oss << num << " ";
        }
        std::string state = oss.str();

        if (seen.find(state) != seen.end()) {
            break;
        }
        seen.insert(state);

        int maxIndex = 0;
        for (int i = 1; i < banks.size(); i++) {
            if (banks[i] > banks[maxIndex]) {
                maxIndex = i;
            }
        }

        int blocks = banks[maxIndex];
        banks[maxIndex] = 0;
        for (int i = 1; i <= blocks; i++) {
            banks[(maxIndex + i) % banks.size()]++;
        }

        cycles++;
    }

    std::cout << "It takes " << cycles << " redistribution cycles to reach a repeated configuration." << std::endl;

    return 0;
}
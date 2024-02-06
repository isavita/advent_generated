#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <unordered_map>

int main() {
    std::ifstream inputFile("input.txt");
    std::vector<int> banks;
    std::unordered_map<std::string, int> seen;
    int cycles = 0;

    if (inputFile.is_open()) {
        std::string line;
        while (std::getline(inputFile, line)) {
            std::stringstream ss(line);
            int num;
            while (ss >> num) {
                banks.push_back(num);
            }
        }
        inputFile.close();
    } else {
        std::cerr << "Unable to open file" << std::endl;
        return 1;
    }

    while (true) {
        std::stringstream stateStream;
        for (int bank : banks) {
            stateStream << bank << " ";
        }
        std::string state = stateStream.str();

        if (seen.find(state) != seen.end()) {
            std::cout << "The size of the loop is " << cycles - seen[state] << std::endl;
            return 0;
        }
        seen[state] = cycles;

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

    return 0;
}
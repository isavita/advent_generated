#include <iostream>
#include <fstream>
#include <unordered_map>

int itemPriority(char item) {
    if (item >= 'a' && item <= 'z') {
        return int(item - 'a' + 1);
    }
    return int(item - 'A') + 27;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    int sum = 0;
    std::string line;
    while (std::getline(file, line)) {
        int half = line.length() / 2;
        std::string firstCompartment = line.substr(0, half);
        std::string secondCompartment = line.substr(half);

        std::unordered_map<char, int> compartmentMap;
        for (char item : firstCompartment) {
            compartmentMap[item]++;
        }
        for (char item : secondCompartment) {
            if (compartmentMap.find(item) != compartmentMap.end()) {
                sum += itemPriority(item);
                break;
            }
        }
    }

    file.close();

    std::cout << sum << std::endl;

    return 0;
}

#include <iostream>
#include <fstream>
#include <unordered_map>

int itemPriority(char item) {
    if (item >= 'a' && item <= 'z') {
        return int(item - 'a') + 1;
    }
    return int(item - 'A') + 27;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    int sum = 0, groupLineCounter = 0;
    std::unordered_map<char, int> groupItems[3];

    std::string line;
    while (std::getline(file, line)) {
        std::unordered_map<char, int> itemsMap;
        for (char item : line) {
            itemsMap[item]++;
        }
        groupItems[groupLineCounter] = itemsMap;
        groupLineCounter++;

        if (groupLineCounter == 3) {
            std::unordered_map<char, int> commonItems;
            for (auto& item : groupItems[0]) {
                if (groupItems[1][item.first] > 0 && groupItems[2][item.first] > 0) {
                    commonItems[item.first]++;
                }
            }
            for (auto& item : commonItems) {
                sum += itemPriority(item.first);
                break; // Since we need only one common item per group
            }
            groupLineCounter = 0;
        }
    }

    file.close();

    std::cout << sum << std::endl;

    return 0;
}

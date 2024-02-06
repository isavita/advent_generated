
#include <iostream>
#include <fstream>
#include <vector>

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<std::string> lines;
    std::string line;
    while (std::getline(file, line)) {
        lines.push_back(line);
    }

    std::vector<std::vector<int> > slopes = {
        {1, 1},
        {3, 1},
        {5, 1},
        {7, 1},
        {1, 2}
    };

    int product = 1;
    for (const auto& slope : slopes) {
        int treeCount = 0;
        int pos = 0;
        for (int i = 0; i < lines.size(); i += slope[1]) {
            if (lines[i][pos] == '#') {
                treeCount++;
            }
            pos = (pos + slope[0]) % lines[i].size();
        }
        product *= treeCount;
    }

    std::cout << product << std::endl;

    return 0;
}

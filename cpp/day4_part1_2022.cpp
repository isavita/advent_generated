
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

std::pair<int, int> parseRange(std::string r) {
    std::stringstream ss(r);
    std::string part;
    std::vector<int> parts;
    while (std::getline(ss, part, '-')) {
        parts.push_back(std::stoi(part));
    }
    return std::make_pair(parts[0], parts[1]);
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    int count = 0;
    std::string line;
    while (std::getline(file, line)) {
        std::stringstream ss(line);
        std::string range;
        std::vector<std::string> ranges;
        while (std::getline(ss, range, ',')) {
            ranges.push_back(range);
        }
        if (ranges.size() != 2) {
            continue;
        }

        auto range1 = parseRange(ranges[0]);
        auto range2 = parseRange(ranges[1]);

        if ((range1.first <= range2.first && range1.second >= range2.second) || 
            (range2.first <= range1.first && range2.second >= range1.second)) {
            count++;
        }
    }

    file.close();

    std::cout << count << std::endl;

    return 0;
}

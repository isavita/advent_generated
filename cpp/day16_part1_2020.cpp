
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>
#include <algorithm>
#include <utility>

std::vector<std::string> split(const std::string& s, const std::string& delimiter) {
    std::vector<std::string> tokens;
    size_t lastPos = 0;
    size_t pos = s.find(delimiter, 0);

    while (pos != std::string::npos) {
        tokens.push_back(s.substr(lastPos, pos - lastPos));
        lastPos = pos + delimiter.length();
        pos = s.find(delimiter, lastPos);
    }
    tokens.push_back(s.substr(lastPos));
    return tokens;
}

long long solve() {
    std::ifstream file("input.txt");
    std::string line;
    
    int maxVal = 0;
    std::vector<std::pair<int, int>> allRanges;

    while (std::getline(file, line) && !line.empty()) {
        size_t colonPos = line.find(":");
        std::string rangesStr = line.substr(colonPos + 2);

        std::vector<std::string> orParts = split(rangesStr, " or ");
        for (const std::string& part : orParts) {
            size_t hyphenPos = part.find("-");
            int start = std::stoi(part.substr(0, hyphenPos));
            int end = std::stoi(part.substr(hyphenPos + 1));
            allRanges.push_back({start, end});
            if (end > maxVal) {
                maxVal = end;
            }
        }
    }

    std::vector<bool> possibleValidValues(maxVal + 1, false);
    for (const auto& range : allRanges) {
        for (int i = range.first; i <= range.second; ++i) {
            possibleValidValues[i] = true;
        }
    }

    std::getline(file, line); 
    std::getline(file, line); 
    std::getline(file, line); 
    
    std::getline(file, line); 
    
    long long errorRate = 0;
    while (std::getline(file, line)) {
        std::vector<std::string> valuesStr = split(line, ",");
        for (const std::string& sVal : valuesStr) {
            int value = std::stoi(sVal);
            if (value < 0 || value > maxVal || !possibleValidValues[value]) {
                errorRate += value;
            }
        }
    }
    file.close();
    return errorRate;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::cout << solve() << std::endl;

    return 0;
}


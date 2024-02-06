
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>
#include <unordered_map>

bool isRealRoom(std::string room);
int getSectorID(std::string room);

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    int sumOfSectorIDs = 0;
    std::string line;
    while (std::getline(file, line)) {
        if (isRealRoom(line)) {
            sumOfSectorIDs += getSectorID(line);
        }
    }

    std::cout << sumOfSectorIDs << std::endl;
    return 0;
}

bool isRealRoom(std::string room) {
    std::vector<std::pair<char, int>> letterCounts;
    std::string checksum;
    std::string encryptedName;
    size_t pos = room.find('[');
    checksum = room.substr(pos + 1, room.size() - pos - 2);
    encryptedName = room.substr(0, pos);
    encryptedName = encryptedName.substr(0, encryptedName.size() - 3);

    std::unordered_map<char, int> counts;
    for (char c : encryptedName) {
        if (c != '-') {
            counts[c]++;
        }
    }

    for (auto it = counts.begin(); it != counts.end(); ++it) {
        letterCounts.push_back(std::make_pair(it->first, it->second));
    }

    std::sort(letterCounts.begin(), letterCounts.end(), [](const std::pair<char, int>& a, const std::pair<char, int>& b) {
        if (a.second == b.second) {
            return a.first < b.first;
        }
        return a.second > b.second;
    });

    for (size_t i = 0; i < checksum.size(); i++) {
        if (checksum[i] != letterCounts[i].first) {
            return false;
        }
    }

    return true;
}

int getSectorID(std::string room) {
    size_t pos = room.find_last_of('-');
    std::string sectorIDPart = room.substr(pos + 1, room.find('[') - pos - 1);
    return std::stoi(sectorIDPart);
}

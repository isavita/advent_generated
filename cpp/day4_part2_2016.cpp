
#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <unordered_map>
#include <string>

bool isRealRoom(std::string room);
int getSectorID(std::string room);
std::string decryptName(std::string room);

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::string line;
    while (std::getline(file, line)) {
        if (isRealRoom(line)) {
            std::string decryptedName = decryptName(line);
            if (decryptedName.find("northpole object") != std::string::npos) {
                std::cout << getSectorID(line) << std::endl;
                break;
            }
        }
    }

    return 0;
}

bool isRealRoom(std::string room) {
    std::vector<std::pair<char, int>> counts;
    std::string checksum;
    size_t openBracketPos = room.find("[");
    checksum = room.substr(openBracketPos + 1, room.size() - openBracketPos - 2);
    
    std::string encryptedName = room.substr(0, openBracketPos);
    size_t lastDashPos = encryptedName.find_last_of("-");
    encryptedName = encryptedName.substr(0, lastDashPos);

    std::unordered_map<char, int> letterCounts;
    for (char c : encryptedName) {
        if (c != '-') {
            letterCounts[c]++;
        }
    }

    for (auto& entry : letterCounts) {
        counts.push_back(std::make_pair(entry.first, entry.second));
    }

    std::sort(counts.begin(), counts.end(), [](const std::pair<char, int>& a, const std::pair<char, int>& b) {
        return a.second > b.second || (a.second == b.second && a.first < b.first);
    });

    for (size_t i = 0; i < checksum.size(); ++i) {
        if (checksum[i] != counts[i].first) {
            return false;
        }
    }

    return true;
}

int getSectorID(std::string room) {
    size_t lastDashPos = room.find_last_of("-");
    size_t openBracketPos = room.find("[");
    std::string sectorIDPart = room.substr(lastDashPos + 1, openBracketPos - lastDashPos - 1);
    return std::stoi(sectorIDPart);
}

std::string decryptName(std::string room) {
    size_t lastDashPos = room.find_last_of("-");
    size_t openBracketPos = room.find("[");
    std::string sectorIDPart = room.substr(lastDashPos + 1, openBracketPos - lastDashPos - 1);
    int sectorID = std::stoi(sectorIDPart);
    
    std::string encryptedName = room.substr(0, lastDashPos);
    std::string decryptedName = "";
    
    for (char c : encryptedName) {
        if (c == '-') {
            decryptedName += " ";
        } else {
            char shiftedLetter = 'a' + ((c - 'a' + sectorID) % 26);
            decryptedName += shiftedLetter;
        }
    }

    return decryptedName;
}

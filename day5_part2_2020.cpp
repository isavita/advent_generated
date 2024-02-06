
#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>

int decode(std::string pass);
int binaryToInt(std::string binaryStr);

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<int> seatIDs;

    std::string line;
    while (std::getline(file, line)) {
        std::replace(line.begin(), line.end(), 'F', '0');
        std::replace(line.begin(), line.end(), 'B', '1');
        std::replace(line.begin(), line.end(), 'L', '0');
        std::replace(line.begin(), line.end(), 'R', '1');
        int seatID = decode(line);
        seatIDs.push_back(seatID);
    }

    std::sort(seatIDs.begin(), seatIDs.end());

    for (size_t i = 0; i < seatIDs.size() - 1; i++) {
        if (seatIDs[i + 1] != seatIDs[i] + 1) {
            std::cout << seatIDs[i] + 1 << std::endl;
            break;
        }
    }

    return 0;
}

int decode(std::string pass) {
    int row = binaryToInt(pass.substr(0, 7));
    int column = binaryToInt(pass.substr(7));
    return row * 8 + column;
}

int binaryToInt(std::string binaryStr) {
    int result = 0;
    for (size_t i = 0; i < binaryStr.size(); i++) {
        if (binaryStr[i] == '1') {
            result |= 1 << (binaryStr.size() - i - 1);
        }
    }
    return result;
}

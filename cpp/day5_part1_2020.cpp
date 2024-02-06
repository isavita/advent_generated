
#include <iostream>
#include <fstream>
#include <string>

int binaryToInt(std::string binaryStr) {
    int result = 0;
    for (int i = 0; i < binaryStr.length(); i++) {
        if (binaryStr[i] == '1') {
            result |= 1 << (binaryStr.length() - i - 1);
        }
    }
    return result;
}

int decode(std::string pass) {
    int row = binaryToInt(pass.substr(0, 7));
    int column = binaryToInt(pass.substr(7));
    return row * 8 + column;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    std::string pass;
    int maxSeatID = 0;
    while (std::getline(file, pass)) {
        std::replace(pass.begin(), pass.end(), 'F', '0');
        std::replace(pass.begin(), pass.end(), 'B', '1');
        std::replace(pass.begin(), pass.end(), 'L', '0');
        std::replace(pass.begin(), pass.end(), 'R', '1');
        int seatID = decode(pass);
        if (seatID > maxSeatID) {
            maxSeatID = seatID;
        }
    }

    std::cout << maxSeatID << std::endl;

    return 0;
}

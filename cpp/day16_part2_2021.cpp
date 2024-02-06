
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>

std::string hexToBin(std::string hex) {
    std::string bin = "";
    for (char h : hex) {
        int b = std::stoi(std::string(1, h), nullptr, 16);
        std::stringstream ss;
        ss << std::bitset<4>(b);
        bin += ss.str();
    }
    return bin;
}

std::tuple<int, int, int64_t> parsePacket(std::string binStr, int idx);

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error reading file" << std::endl;
        return 1;
    }

    std::string hexStr;
    std::getline(file, hexStr);
    file.close();

    std::string binStr = hexToBin(hexStr);
    auto [_, __, value] = parsePacket(binStr, 0);
    std::cout << value << std::endl;

    return 0;
}

std::tuple<int, int, int64_t> parsePacket(std::string binStr, int idx) {
    int version = (binStr[idx] - '0') << 2 | (binStr[idx + 1] - '0') << 1 | (binStr[idx + 2] - '0');
    int typeID = (binStr[idx + 3] - '0') << 2 | (binStr[idx + 4] - '0') << 1 | (binStr[idx + 5] - '0');
    idx += 6;

    if (typeID == 4) {
        int64_t value = 0;
        while (binStr[idx] == '1') {
            value = (value << 4) | (binStr[idx + 1] - '0') << 3 | (binStr[idx + 2] - '0') << 2 | (binStr[idx + 3] - '0') << 1 | (binStr[idx + 4] - '0');
            idx += 5;
        }
        value = (value << 4) | (binStr[idx + 1] - '0') << 3 | (binStr[idx + 2] - '0') << 2 | (binStr[idx + 3] - '0') << 1 | (binStr[idx + 4] - '0');
        idx += 5;
        return {version, idx, value};
    }

    int lengthTypeID = binStr[idx] - '0';
    idx++;
    int numSubPackets = 0, subPacketLength = 0;

    if (lengthTypeID == 0) {
        subPacketLength = 0;
        for (int i = 0; i < 15; i++) {
            subPacketLength = (subPacketLength << 1) | (binStr[idx] - '0');
            idx++;
        }
    } else {
        numSubPackets = 0;
        for (int i = 0; i < 11; i++) {
            numSubPackets = (numSubPackets << 1) | (binStr[idx] - '0');
            idx++;
        }
    }

    std::vector<int64_t> values;
    while (true) {
        if ((lengthTypeID == 0 && subPacketLength == 0) || (lengthTypeID == 1 && numSubPackets == 0)) {
            break;
        }
        auto [_, newIndex, subValue] = parsePacket(binStr, idx);
        values.push_back(subValue);

        if (lengthTypeID == 0) {
            subPacketLength -= newIndex - idx;
        } else {
            numSubPackets--;
        }
        idx = newIndex;
    }

    int64_t result = 0;
    switch (typeID) {
        case 0:
            for (int64_t value : values) {
                result += value;
            }
            break;
        case 1:
            result = 1;
            for (int64_t value : values) {
                result *= value;
            }
            break;
        case 2:
            result = values[0];
            for (int i = 1; i < values.size(); i++) {
                if (values[i] < result) {
                    result = values[i];
                }
            }
            break;
        case 3:
            result = values[0];
            for (int i = 1; i < values.size(); i++) {
                if (values[i] > result) {
                    result = values[i];
                }
            }
            break;
        case 5:
            result = (values[0] > values[1]) ? 1 : 0;
            break;
        case 6:
            result = (values[0] < values[1]) ? 1 : 0;
            break;
        case 7:
            result = (values[0] == values[1]) ? 1 : 0;
            break;
        default:
            throw std::runtime_error("Unknown typeID");
    }

    return {version, idx, result};
}

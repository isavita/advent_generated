#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <bitset>

std::string hexToBin(std::string hex) {
    std::string bin = "";
    for (char h : hex) {
        int b = std::stoi(std::string(1, h), 0, 16);
        bin += std::bitset<4>(b).to_string();
    }
    return bin;
}

std::pair<int, int> parsePacket(std::string binStr, int idx) {
    int version = (binStr[idx]-'0')<<2 | (binStr[idx+1]-'0')<<1 | (binStr[idx+2]-'0');
    int typeID = (binStr[idx+3]-'0')<<2 | (binStr[idx+4]-'0')<<1 | (binStr[idx+5]-'0');
    idx += 6;

    if (typeID == 4) {
        while (binStr[idx] == '1') {
            idx += 5;
        }
        idx += 5;
        return std::make_pair(version, idx);
    }

    int lengthTypeID = binStr[idx] - '0';
    idx++;
    int numSubPackets = 0, subPacketLength = 0;

    if (lengthTypeID == 0) {
        subPacketLength = 0;
        for (int i = 0; i < 15; i++) {
            subPacketLength = (subPacketLength<<1) | (binStr[idx]-'0');
            idx++;
        }
    } else {
        numSubPackets = 0;
        for (int i = 0; i < 11; i++) {
            numSubPackets = (numSubPackets<<1) | (binStr[idx]-'0');
            idx++;
        }
    }

    int versionSum = version;
    while (true) {
        if ((lengthTypeID == 0 && subPacketLength == 0) || (lengthTypeID == 1 && numSubPackets == 0)) {
            break;
        }
        auto [subVersion, newIndex] = parsePacket(binStr, idx);
        versionSum += subVersion;

        if (lengthTypeID == 0) {
            subPacketLength -= newIndex - idx;
        } else {
            numSubPackets--;
        }
        idx = newIndex;
    }
    return std::make_pair(versionSum, idx);
}

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
    auto [versionSum, _] = parsePacket(binStr, 0);
    std::cout << versionSum << std::endl;

    return 0;
}
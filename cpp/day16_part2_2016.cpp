
#include <iostream>
#include <fstream>
#include <string>

const int diskLength = 35651584;

std::string readInitialState(std::string filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Failed to open file");
    }

    std::string initialState;
    getline(file, initialState);
    file.close();

    return initialState;
}

std::string generateData(std::string initialState, int length) {
    std::string data = initialState;
    while (data.length() < length) {
        std::string b;
        for (int i = data.length() - 1; i >= 0; i--) {
            if (data[i] == '0') {
                b.push_back('1');
            } else {
                b.push_back('0');
            }
        }
        data = data + "0" + b;
    }
    return data.substr(0, length);
}

std::string calculateChecksum(std::string data) {
    while (data.length() % 2 == 0) {
        std::string b;
        for (size_t i = 0; i < data.length(); i += 2) {
            if (data[i] == data[i + 1]) {
                b.push_back('1');
            } else {
                b.push_back('0');
            }
        }
        data = b;
    }
    return data;
}

int main() {
    std::string initialState = readInitialState("input.txt");
    std::string data = generateData(initialState, diskLength);
    std::string checksum = calculateChecksum(data);
    std::cout << "Checksum: " << checksum << std::endl;
    return 0;
}

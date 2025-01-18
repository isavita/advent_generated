
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>

using namespace std;

inline uint64_t nextSecret(uint64_t s) {
    s ^= (s * 64);
    s &= 0xFFFFFF;
    s ^= (s / 32);
    s &= 0xFFFFFF;
    s ^= (s * 2048);
    s &= 0xFFFFFF;
    return s;
}

int main() {
    ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        cerr << "Error opening input.txt" << endl;
        return 1;
    }

    vector<uint64_t> buyers;
    string line;
    while (getline(inputFile, line)) {
        if (line.empty()) continue;
        uint64_t n;
        stringstream ss(line);
        ss >> n;
        buyers.push_back(n);
    }
    inputFile.close();

    uint64_t total = 0;
    for (uint64_t b : buyers) {
        uint64_t s = b;
        for (int i = 0; i < 2000; ++i) {
            s = nextSecret(s);
        }
        total += s;
    }

    cout << total << endl;

    return 0;
}

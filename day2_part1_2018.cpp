#include <iostream>
#include <fstream>
#include <unordered_map>

using namespace std;

pair<bool, bool> countTwosAndThrees(string id) {
    unordered_map<char, int> charCount;
    for (char c : id) {
        charCount[c]++;
    }

    bool hasTwos = false, hasThrees = false;
    for (auto& pair : charCount) {
        if (pair.second == 2) {
            hasTwos = true;
        } else if (pair.second == 3) {
            hasThrees = true;
        }
    }
    return make_pair(hasTwos, hasThrees);
}

int main() {
    ifstream file("input.txt");
    if (!file.is_open()) {
        cerr << "Error opening file" << endl;
        return 1;
    }

    int twoCount = 0, threeCount = 0;
    string id;
    while (getline(file, id)) {
        auto counts = countTwosAndThrees(id);
        if (counts.first) {
            twoCount++;
        }
        if (counts.second) {
            threeCount++;
        }
    }

    file.close();

    int checksum = twoCount * threeCount;
    cout << checksum << endl;

    return 0;
}
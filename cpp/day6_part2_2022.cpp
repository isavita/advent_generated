
#include <iostream>
#include <fstream>
#include <string>
#include <unordered_map>

int firstNUnique(const std::string& s, int n) {
    for (int i = n; i < s.length(); i++) {
        std::string sub = s.substr(i - n, n);
        std::unordered_map<char, bool> m;
        bool isUnique = true;
        for (char c : sub) {
            if (m.find(c) != m.end()) {
                isUnique = false;
                break;
            }
            m[c] = true;
        }
        if (isUnique) {
            return i;
        }
    }
    return -1;
}

std::string readAll(const std::string& path) {
    std::ifstream file(path);
    std::string content((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    return content;
}

int main() {
    std::string s = readAll("input.txt");
    std::cout << firstNUnique(s, 14) << std::endl;
    return 0;
}

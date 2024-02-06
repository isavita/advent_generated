#include <iostream>
#include <fstream>
#include <string>
#include <unordered_set>

int firstNUnique(const std::string& s, int n) {
    for (int i = n; i < s.length(); i++) {
        std::string sub = s.substr(i - n, n);
        std::unordered_set<char> unique(sub.begin(), sub.end());
        if (unique.size() == n) {
            return i;
        }
    }
    return -1;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::string s;
    std::getline(file, s);
    file.close();

    std::cout << firstNUnique(s, 4) << std::endl;

    return 0;
}

#include <iostream>
#include <fstream>
#include <regex>
#include <vector>

bool supportsSSL(std::string ip);
std::vector<std::string> findABAs(std::string s);

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    int sslCount = 0;
    std::string line;
    while (std::getline(file, line)) {
        if (supportsSSL(line)) {
            sslCount++;
        }
    }

    std::cout << sslCount << std::endl;

    return 0;
}

bool supportsSSL(std::string ip) {
    std::regex insideBrackets("\\[[a-z]+\\]");
    std::vector<std::string> bracketContents;
    auto words_begin = std::sregex_iterator(ip.begin(), ip.end(), insideBrackets);
    auto words_end = std::sregex_iterator();
    for (std::sregex_iterator i = words_begin; i != words_end; ++i) {
        bracketContents.push_back(i->str());
    }

    // Generate BABs from ABAs found outside brackets
    ip = std::regex_replace(ip, insideBrackets, "-");
    for (const auto& aba : findABAs(ip)) {
        std::string bab = std::string(1, aba[1]) + std::string(1, aba[0]) + std::string(1, aba[1]);
        for (const auto& bracketContent : bracketContents) {
            if (bracketContent.find(bab) != std::string::npos) {
                return true;
            }
        }
    }

    return false;
}

std::vector<std::string> findABAs(std::string s) {
    std::vector<std::string> abas;
    for (size_t i = 0; i < s.length() - 2; i++) {
        if (s[i] != s[i + 1] && s[i] == s[i + 2]) {
            abas.push_back(s.substr(i, 3));
        }
    }
    return abas;
}

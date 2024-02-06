#include <iostream>
#include <fstream>
#include <string>
#include <unordered_map>
#include <regex>

int main() {
    std::ifstream file("input.txt");
    std::string line;
    std::unordered_map<std::string, bool> holderMap;
    std::unordered_map<std::string, bool> heldMap;
    std::regex re("[a-z]+");

    if (file.is_open()) {
        while (std::getline(file, line)) {
            std::sregex_iterator it(line.begin(), line.end(), re);
            std::sregex_iterator end;

            std::vector<std::string> names;
            for (; it != end; ++it) {
                names.push_back(it->str());
            }

            std::string holder = names[0];
            holderMap[holder] = true;

            if (names.size() > 1) {
                for (size_t i = 1; i < names.size(); ++i) {
                    heldMap[names[i]] = true;
                }
            }
        }
        file.close();

        for (const auto& pair : holderMap) {
            if (heldMap.find(pair.first) == heldMap.end()) {
                std::cout << pair.first << std::endl;
                return 0;
            }
        }
    } else {
        std::cout << "File reading error" << std::endl;
    }

    return 0;
}
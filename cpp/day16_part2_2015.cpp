
#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <sstream>

int main() {
    std::map<std::string, int> mfcsam_data = {
        {"children", 3},
        {"cats", 7},
        {"samoyeds", 2},
        {"pomeranians", 3},
        {"akitas", 0},
        {"vizslas", 0},
        {"goldfish", 5},
        {"trees", 3},
        {"cars", 2},
        {"perfumes", 1}
    };

    std::ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        return 1;
    }

    std::string line;
    while (std::getline(inputFile, line)) {
        std::stringstream ss(line);
        std::string token;

        ss >> token;
        ss >> token;
        int sue_num = std::stoi(token.substr(0, token.length() - 1));

        bool valid = true;
        while (ss >> token) {
            std::string prop_name = token.substr(0, token.length() - 1);
            ss >> token;
            int prop_value = std::stoi(token);

            if (prop_name == "cats" || prop_name == "trees") {
                if (mfcsam_data[prop_name] >= prop_value) {
                    valid = false;
                    break;
                }
            } else if (prop_name == "pomeranians" || prop_name == "goldfish") {
                if (mfcsam_data[prop_name] <= prop_value) {
                    valid = false;
                    break;
                }
            } else {
                if (mfcsam_data[prop_name] != prop_value) {
                    valid = false;
                    break;
                }
            }
        }

        if (valid) {
            std::cout << sue_num << std::endl;
        }
    }

    return 0;
}

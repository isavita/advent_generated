#include <iostream>
#include <fstream>
#include <vector>
#include <unordered_map>
#include <regex>

struct BagRule {
    std::string Color;
    int Count;
};

int countBags(std::string color, std::unordered_map<std::string, std::vector<BagRule>> &rules) {
    int count = 1;
    for (const auto &rule : rules[color]) {
        count += rule.Count * countBags(rule.Color, rules);
    }
    return count;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    std::unordered_map<std::string, std::vector<BagRule>> rules;
    std::regex ruleRegex("(\\d+) (\\w+ \\w+) bags?[,.]");

    std::string line;
    while (std::getline(file, line)) {
        size_t pos = line.find(" bags contain ");
        std::string container = line.substr(0, pos);
        std::string contents = line.substr(pos + 14);

        if (contents == "no other bags.") {
            continue;
        }

        std::smatch match;
        std::string::const_iterator searchStart(contents.cbegin());
        while (std::regex_search(searchStart, contents.cend(), match, ruleRegex)) {
            int count = std::stoi(match[1]);
            rules[container].push_back({match[2], count});
            searchStart = match.suffix().first;
        }
    }

    int totalBags = countBags("shiny gold", rules) - 1;
    std::cout << totalBags << std::endl;

    return 0;
}